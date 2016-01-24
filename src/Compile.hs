{-# LANGUAGE FlexibleContexts #-}
module Compile where

import Data.Map as M
import Control.Monad.State
import LatteState
import AbsLatte
import Common
import Predefined
import TypeChecker

data AProgram = AProgram [ADef] deriving (Eq, Ord)

data ADef = AFun String [Instr] | AStr String String | AClass deriving (Eq, Ord)

data Instr = 
	  Instr AsOp AType Object Object
	| InstrS AsSOp AType Object
	| ARet
	| Cdq
	| Cltq
	| Jump RelOp Label
	| Goto Label
	| Call String
	| Syscall
	| Leave
	| LabelDecl Label deriving (Eq, Ord)

data Label = Label String deriving (Eq, Ord)

data AsOp = AMov | AAdd | ASub | AMul | ADiv | ACmp | ALea deriving (Eq, Ord)
data AsSOp = Sel | Ser | Push | Pop | ANeg deriving (Eq, Ord)

data AType = Byte | Word | DWord | QWord deriving (Eq, Ord)

showl items = Prelude.foldl (++) "" (Prelude.map (show) items)

instance Show AProgram where
	show (AProgram defs) = (showl defs)

instance Show ADef where
	show (AStr strId str) = (id strId) ++ ":\n" ++ "\t .string " ++ (show str) ++ "\n"
	show (AFun name instrs) = "\t .global " ++ (id name) ++ "\n" ++  (id name) ++ ":\n" ++ showl instrs
	show (AClass) = ""
instance Show Instr where
	show (ARet) = "\tret\n"
	show (LabelDecl label) = (show label) ++ ":\n"
	show (Instr op typ object1 object2) = "\t" ++ (show op) ++ (show typ) ++ "\t" ++ (showObj typ object1) ++ ", " ++ (showObj typ object2) ++ "\n"
	show (InstrS op typ object) = "\t" ++ (show op) ++ (show typ) ++ "\t" ++ (showObj typ object) ++ "\n"
	show (Jump op label) = "\t" ++ (jumpOp op) ++ " "++ (show label) ++ "\n"
	show (Goto label) = "\tjmp " ++ (show label) ++ "\n"
	show (Call ident) = "\tcall\t" ++ (id ident) ++ "\n"
	show (Leave) = "\tleave\n"
	show (Syscall) = "\tsyscall\n"
	show (Cdq) = "\tcdq\n"
	show (Cltq) = "\tcltq\n"

instance Show Label where
 	show (Label label) = (id label)

instance Show AsOp where
	show AMov = "mov" 
	show AAdd = "add" 
	show ASub = "sub" 
	show AMul = "imul" 
	show ADiv = "idiv" 
	show ACmp = "cmp"
	show ALea = "lea"

instance Show  AsSOp where
	show Sel = "sel"
	show Ser = "ser"
	show Push = "push"
	show Pop = "pop"
	show ANeg = "neg"

instance Show AType where
	show Byte = "b"
	show Word = "w"
	show DWord = "l"
	show QWord = "q"

aType :: Type -> AType
aType Int = DWord
aType Bool = Byte
aType _ = QWord

addOpToAs Plus = AAdd
addOpToAs Minus = ASub

getVar ident = do
	S (context, state, funInfo, cInfo, label, regs, pResults, cStr) <- get
	var <- getVar' ident $ variables funInfo
	return var
	where
		getVar' _ [] = fail $ "Variable " ++ (show ident) ++ "is not defined."
		getVar' ident (vEnv:vx) = do
			case M.lookup ident vEnv of
				(Just var) -> return var
				Nothing -> do
					fromLower <- getVar' ident vx
					return fromLower

getNextLabel :: MonadState LState m => m String
getNextLabel = do
	S (context, state, funInfo, cInfo, label, regs, pResults, cStr) <- get
	put $ S (context, state, funInfo, cInfo, label + 1, regs, pResults, cStr)
	return $ ".L" ++ (show label)

getNextLabel' :: MonadState LState m => m Label
getNextLabel' = do
	S (context, state, funInfo, cInfo, label, regs, pResults, cStr) <- get
	put $ S (context, state, funInfo, cInfo, label + 1, regs, pResults, cStr)
	return $ Label $ ".L" ++ (show label)

showObj :: AType -> Object -> String
showObj _ Dummy = ""
showObj _ (Con i) = "$" ++ (show i)
showObj _ (ConS s) = "$" ++ (id s)
showObj _ (MemIO name io) = (id io) ++ "(%" ++ (id name) ++ ")"
showObj typ (MemS reg shift) = (showShift shift) ++ "(" ++ (showObj QWord reg) ++ ")"
showObj typ (MemD base shift multiplier shift2) = (showShift shift2) ++ "(" ++ (showObj QWord base) ++ "," ++ (showObj QWord shift) ++ "," ++ (show multiplier) ++ ")"

showObj Byte reg = "%" ++ (id $ r8 reg)
showObj DWord reg = "%" ++ (id $ r32 reg)
showObj QWord reg = "%" ++ (id $ r64 reg)
showObj Word reg = "%" ++ (id $ r64 reg)

showShift i = if i == 0 then "" else (show i)

jumpOp LTH = "jl"
jumpOp LE = "jle"
jumpOp GTH = "jg"
jumpOp GE = "jge"
jumpOp EQU = "je"
jumpOp NE = "jne"

toReg (VarInfo typ shift loc) = (MemS rbpReg shift)
toReg (ClassVar typ shift) = (MemS r12Reg shift)

getCharSize Int = return "l"
getCharSize Bool = return "b"
getCharSize _ = return "q"

genProg' (Program topDefs) = do
	strDefs <- getStrDefs
	sDecls <- forM (strDefs ++ predefinedString) genStrDecl'
	funDefs <- forM topDefs genTopDef'
	return (AProgram $ sDecls ++ (concat funDefs))

genTopDef' (TopFun funDef) = do
	funDef' <- genFun' funDef
	return [funDef']

genTopDef' (ClassDef ident classBlock) = do
	afuns <- runForClass ident $ genClassBlock classBlock
	return afuns

genTopDef' (ClassDefE ident base classBlock) = do
	afuns <- runForClass ident $ genClassBlock classBlock
	return afuns

genClassBlock (ClassBlock elems) = do
	afuns <- forM elems genClassElem
	return $ afuns

genClassElem (ClassFun funDef) = do
	afun <- genFun' funDef
	return afun

genClassElem _ = do
	return AClass

makeProlog args addshift usedRegs = do
	(instr, _, _) <- foldM genArgInstr ([], 0, regCallOrder) args
	savedRegisters <- forM (Prelude.filter (\r -> elem r usedRegs) toSave) (\r -> return (InstrS Push QWord r))
	localVarShift <- getTotalShift
	return $ [InstrS Push QWord rbpReg,Instr AMov QWord rspReg rbpReg] ++ [Instr ASub QWord (Con $ (addshift + (toInteger $ ((length savedRegisters) `mod` 2) * 8) + addjustTo16 (-localVarShift))) rspReg] ++ savedRegisters ++ (reverse instr)

genArgInstr (instr, i, []) _ = return (instr, i, [])
genArgInstr (instr, i, (r:freeRegs)) (Arg _ ident) = do
	varInfo <- getVar ident
	return ((Instr AMov (aType $ vType varInfo) r (MemS  rbpReg (shift varInfo)):instr), i, freeRegs)

makeEpilog args addshift usedRegs = do
	savedRegisters <- forM (Prelude.filter (\r -> elem r usedRegs) toSave) (\r -> return (InstrS Pop QWord r))
	localVarShift <- getTotalShift
	ident <- getFunFromContext
	return $ [LabelDecl $ toEndLabel ident] ++ (reverse savedRegisters) ++ [Instr AAdd QWord (Con $ (addshift + (toInteger $ ((length savedRegisters) `mod` 2) * 8) + addjustTo16 (-localVarShift))) rspReg] ++ [Instr AMov QWord rbpReg rspReg, InstrS Pop QWord rbpReg, ARet]

genFun' f@(FnDef typ i@(Ident ident) args block) = do
	ret <- runForFun i $ genFun f
	return ret

genFun f@(FnDef typ i@(Ident ident) args block) = do
		args <- addOwnerObjectToArgs args
		_ <- (foldM declParam (1) args)
		instrs <- getBody f
		addjustShift
		shift <- foldM countRspShift 0 instrs
		objectPlacement <- placeObject
		usedRegs <- foldM collectUsedSafeRegs [] (objectPlacement ++ instrs)
		prolog <- makeProlog args (addjustTo16 shift) usedRegs
		epilog <- makeEpilog args (addjustTo16 shift) usedRegs
		return (AFun ident (prolog ++ objectPlacement ++ instrs ++ epilog))
	where
		declParam no (Arg typ ident) = do
			if no <= 6 then do
				decl ident typ
				return $ no + 1
			else do
				declAt ((no - 5) * 8) ident typ
				return $ no + 1

getBody f@(FnDef typ i@(Ident ident) args block) = 
	if isPredefined ident then do
		getPredefined ident
	else do
		genBlock' block

genBlock' (Block stmts) = do
	insts <- runForBlock $ forM stmts genStmt'
	return $ concat insts

countRspShift prev (Instr asOp typ o1 (MemS rspReg i)) = return $ max prev i
countRspShift prev _ = return prev

countSubShift prev (InstrS Push _ _) = do
	if prev == 8 then return 0
		else return 8

countSubShift prev _ = return prev

collectUsedSafeRegs used (Instr _ _ _ r) = do
	if and [elem r toSave,not $ elem r used] then
		return $ r:used
	else
		return used

collectUsedSafeRegs used _ = return used


-- Statements

genStmt' :: MonadState LState m => Stmt -> m [Instr]
genStmt' Empty = return []

genStmt' (BStmt block) = do
	instr <- genBlock' block
	return instr

genStmt' (Decl typ items) = do
	instr <- forM items $ declareVar' typ
	return $ concat instr

genStmt' (Ass ident expr) = do
	varInfo <- getVar ident
	eInstr <- genExpr' expr
	return $ eInstr ++ [Instr AMov (aType $ vType varInfo) raxReg (toReg varInfo)]

genStmt' (ArrAss arrAccess expr) = do
	typ <- getType arrAccess
	eInstr <- genExpr' expr
	(sInstr, lInstr, reg, typ') <- storeTmp typ raxReg
	(aInstrs, _) <- genArrAccess' arrAccess
	releaseTmp typ' reg
	return $ eInstr ++ sInstr ++ aInstrs ++ [Instr AMov (aType $ typ) reg (MemS raxReg 0)] ++ lInstr

genStmt' (FldAss fieldAccess expr) = do
	typ <- getType fieldAccess
	eInstr <- genExpr' expr
	(sInstr, lInstr, reg, typ') <- storeTmp typ raxReg
	(fInstrs, _) <- genFldAccess' fieldAccess
	releaseTmp typ' reg
	return $ eInstr ++ sInstr ++ fInstrs ++ [Instr AMov (aType $ typ) reg (MemS raxReg 0)] ++ lInstr

genStmt' (Incr ident) = do
	varInfo <- getVar ident
	return [(Instr AAdd (aType Int) (Con 1) (toReg varInfo))]

genStmt' (Decr ident) = do
	varInfo <- getVar ident
	return [(Instr ASub (aType Int) (Con 1) (toReg varInfo))]

genStmt' (Ret expr) = do
	exprInstr <- genExpr' expr
	fLabel <- getFunFromContext
	return $ exprInstr ++ [Goto $ toEndLabel fLabel]

genStmt' (VRet) = do
	fLabel <- getFunFromContext
	return [(Instr AMov (aType Int) (Con 0) raxReg), Goto $ toEndLabel fLabel]

genStmt' (Cond expr stmt) = do
	lTrue <- getNextLabel'
	lAfter <- getNextLabel'
	exprInstr <- genCond' expr lTrue lAfter
	stmtInstr <- genStmt' stmt
	return $ exprInstr ++ [LabelDecl lTrue] ++ stmtInstr ++ [LabelDecl lAfter]

genStmt' (CondElse expr stmt1 stmt2) = do
	lTrue <- getNextLabel'
	lFalse <- getNextLabel'
	lAfter <- getNextLabel'
	eInstr <- genCond' expr lTrue lFalse
	sInstr1 <- genStmt' stmt1
	sInstr2 <- genStmt' stmt2
	return $ eInstr ++ [LabelDecl lTrue] ++ sInstr1 ++ [Goto lAfter, LabelDecl lFalse] ++ sInstr2 ++ [LabelDecl lAfter]

genStmt' (While expr stmt) = do
	lTrue <- getNextLabel'
	lCond <- getNextLabel'
	lFalse <- getNextLabel'
	sInstr <- genStmt' stmt
	eInstr <- genCond' expr lTrue lFalse
	return $ [Goto lCond, LabelDecl lTrue] ++ sInstr ++ [LabelDecl lCond] ++ eInstr ++ [LabelDecl lFalse]

genStmt' s@(For typ ident expr stmt) = do
	ret <- runForBlock $ genStmt s
	return ret
	where
		genStmt s@(For typ ident expr stmt) = do
			decl ident typ
			lStart <- getNextLabel'
			lEnd <- getNextLabel'
			eInstr <- genExpr' expr
			(sInstr, lInstr, reg, typ') <- storeTmp typ raxReg
			(sInstr', lInstr', reg', typ'') <- storeTmp typ raxReg
			varInfo <- getVar ident
			stInstr <- genStmt' stmt
			releaseTmp typ' reg
			releaseTmp typ'' reg'
			return $ eInstr ++ sInstr ++ [Instr AMov QWord (Con 0) raxReg] ++ sInstr' ++ [LabelDecl lStart, Instr AAdd QWord (Con 1) reg', Instr AMov (aType typ) (MemD reg reg' 4 0) raxReg, Instr AMov DWord raxReg (toReg varInfo), Instr ACmp DWord reg' (MemS reg 0), Jump LTH lEnd] ++ stInstr ++ [Goto lStart, LabelDecl lEnd] ++ lInstr' ++ lInstr
			
genStmt' (SExp expr) = do
	eInstr <- genExpr' expr
	return eInstr

declareVar' :: MonadState LState m => Type -> Item -> m [Instr]
declareVar' typ (NoInit ident) = do
	decl ident typ
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	return $ [Instr AMov (aType $ vType varInfo) (Con 0) $ toReg varInfo]

declareVar' typ (Init ident (ELitInt i)) = do
	decl ident typ
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	return $ [Instr AMov (aType $ vType varInfo) (Con i) $ toReg varInfo]

declareVar' typ (Init ident ELitFalse) = do
	decl ident typ
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	return $ [Instr AMov (aType $ vType varInfo) (Con 0) $ toReg varInfo]

declareVar' typ (Init ident ELitTrue) = do
	decl ident typ
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	return $ [Instr AMov (aType $ vType varInfo) (Con 1) $ toReg varInfo]

declareVar' typ (Init ident expr) = do
	exprInstr <- genExpr' expr
	decl ident typ
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	return $ exprInstr ++ [Instr AMov (aType $ vType varInfo) raxReg $ toReg varInfo]

-- ArrAccess

genArrAccess' :: MonadState LState m => ArrAccess -> m ([Instr], [Instr])
genArrAccess' a@(ASimple ident expr) = do
	varInfo <- getVar ident
	typ <- getType a
	eInstr <- genExpr' expr
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	releaseTmp typ' reg
	return (eInstr ++ [Cltq, Instr ALea QWord (MemD Dummy raxReg 4 4) raxReg] ++ sInstr ++ [Instr AMov QWord (toReg varInfo) raxReg, Instr AAdd QWord reg raxReg] ++ lInstr, [Instr AMov (aType $ vType varInfo) (MemS raxReg 0) raxReg])

genArrAccess' a@(AField specExpr expr) = do
	typ <- getType a
	eInstr <- genExpr' expr
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	spInstr <- genSpecExpr' specExpr
	releaseTmp typ' reg
	return $ (eInstr ++ [Cltq, Instr ALea QWord (MemD Dummy raxReg 4 4) raxReg] ++ sInstr ++ spInstr ++ [Instr AAdd QWord reg raxReg] ++ lInstr, [Instr AMov (aType $ typ) (MemS raxReg 0) raxReg])

-- FieldAccess

genFldAccess' :: MonadState LState m => FieldAccess -> m ([Instr], [Instr])
genFldAccess' (FSAcc ident (Ident "length")) = do
	eInstr <- genExpr' (EVar ident)
	return $ (eInstr, [Instr AMov DWord (MemS raxReg 0) raxReg])

genFldAccess' a@(FSAcc ident ident2) = do
	varInfo <- getVar ident
	classInfo <- getClassInfo $ getClassIdent (vType varInfo)
	case M.lookup ident2 (attrs classInfo) of
		(Just varInfo2) -> do
			return ([Instr AMov QWord (toReg varInfo) raxReg, Instr AAdd QWord (Con $ shift varInfo2) raxReg],[Instr AMov (aType $ vType varInfo2) (MemS raxReg 0) raxReg])
		Nothing -> fail $ "ERROR"

genFldAccess' a@(FAcc specExpr ident) = do
	sInstr <- genSpecExpr' specExpr
	typ <- getType specExpr
	classInfo <- getClassInfo $ getClassIdent typ
	case M.lookup ident (attrs classInfo) of
		(Just varInfo2) -> do
			return $ (sInstr ++ [Instr AAdd QWord (Con $ shift varInfo2) raxReg],[Instr AMov (aType $ vType varInfo2) (MemS raxReg 0) raxReg])
		Nothing -> fail $ "ERROR"


genMethCall' :: MonadState LState m => MethCall -> m ([Instr], [Instr])
genMethCall' (MCall oIdent mIdent@(Ident ident) exprs) = do
	varInfo <- getVar oIdent
	method <- getMethod (getClassIdent $ vType varInfo) mIdent
	(store, restore) <- clearUsedRegs
	instr <- callExprs (zip exprs (tail $ getFunArgTypes $ fType method)) (tail regCallOrder) 0
	return $ (store ++ instr ++ [Instr AMov (aType $ vType varInfo) (toReg varInfo) rdiReg, Call ident] ++ restore, [])


--genMethCall' (MSCall specExpr mIdent expr) = do
--	sInstr <- genSpecExpr' specExpr
--	typ <- getType sInstr
--	method <- getMethod (getClassIdent typ) mIdent
--	(store, restore) <- clearUsedRegs
--	instr <- callExprs (zip exprs (tail $ getFunArgTypes $ fType method)) (tail regCallOrder) 0
--	return $ (sInstr ++ store ++ instr ++ [Instr AMov (aType typ) reg rdiReg, Call ident] ++ restore, [])


-- SpecExpr

genSpecExpr' :: MonadState LState m => SpecExp -> m [Instr]
genSpecExpr' (SAcc fieldAccess) = do
	(fInstr, fMov) <- genFldAccess' fieldAccess
	return $ fInstr ++ fMov

genSpecExpr' (SArr arrAccess) = do
	(aInstr, aMov) <- genArrAccess' arrAccess
	return $ aInstr ++ aMov

-- Expresions

genCond' :: MonadState LState m => Expr -> Label -> Label -> m [Instr]
genCond' (ERel expr1 op expr2) lTrue lFalse = do
	eInstr1 <- genExpr' expr1
	typ <- getType expr1
	(sInstr, lInstr, reg, typ') <- storeTmp typ raxReg
	eInstr2 <- genExpr' expr2
	releaseTmp typ' reg
	return $ eInstr1 ++ sInstr ++ eInstr2 ++ [Instr ACmp (aType typ) reg raxReg]  ++ lInstr ++ [Jump (opposite op) lTrue, Goto lFalse]

genCond' (EVar ident) lTrue lFalse = do
	varInfo <- getVar ident
	return [(Instr ACmp (aType $ vType varInfo) (Con 0) (MemS rbpReg (shift varInfo))),(Jump NE lTrue), (Goto lFalse)]

genCond' (EAnd cond1 cond2) lTrue lFalse = do
	midLabel <- getNextLabel'
	cInstr1 <- genCond' cond1 midLabel lFalse
	cInstr2 <- genCond' cond2 lTrue lFalse
	return $ cInstr1 ++ [LabelDecl midLabel] ++ cInstr2

genCond' (EOr cond1 cond2) lTrue lFalse = do
	midLabel <- getNextLabel'
	cInstr1 <- genCond' cond1 lTrue midLabel
	cInstr2 <- genCond' cond2 lTrue lFalse
	return $ cInstr1 ++ [LabelDecl midLabel] ++ cInstr2

genCond' (Not cond) lTrue lFalse = do
	instr <- genCond' cond lFalse lTrue
	return instr

genCond' expr lTrue lFalse = do
	eInstr <- genExpr' expr
	return $ eInstr ++ [Instr ACmp (aType Bool) (Con 0) raxReg, Jump NE lTrue, Goto lFalse]

genExpr' :: MonadState LState m => Expr -> m [Instr]
genExpr' (EVar ident) = do
	varInfo <- getVar ident
	return [(Instr AMov (aType $ vType varInfo) (toReg varInfo) raxReg)]

genExpr' (EObj alloc) = do
	instr <- genAlloc alloc
	return instr

genExpr' (ENullCast typ) = do
	return [Instr AMov  (aType typ) (Con 0) raxReg]

genExpr' (ELitInt i) = do
	return [Instr AMov DWord (Con i) raxReg]

genExpr' ELitTrue = do
	return [Instr AMov Byte (Con 1) raxReg]

genExpr' ELitFalse = do
	return [Instr AMov Byte (Con 0) raxReg]

genExpr' (EApp i@(Ident ident) exprs) = do
	(store, restore) <- clearUsedRegs
	(Fun _ types) <- getFunType i
	instr <- callExprs (zip exprs types) regCallOrder 0
	return $ store ++ instr ++ [Call ident] ++ restore

genExpr' (EString string) = do
	strId <- getStringId string
	return [Instr AMov QWord (ConS strId) raxReg]

genExpr' e@(Not expr) = do
	lTrue <- getNextLabel'
	lFalse <- getNextLabel'
	lAfter <- getNextLabel'
	eInstr <- genCond' e lTrue lFalse
	return $ eInstr ++ [LabelDecl lTrue] ++ [Instr AMov DWord (Con 1) raxReg, Goto lAfter, LabelDecl lFalse] ++ [Instr AMov DWord (Con 0) raxReg] ++ [LabelDecl lAfter]

genExpr' (Neg expr) = do
	eInstr <- genExpr' expr
	return $ eInstr ++ [InstrS ANeg DWord raxReg]

genExpr' (EAdd expr1 Plus expr2) = do
	eInstr1 <- genExpr' expr1
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	eInstr2 <- genExpr' expr2
	releaseTmp typ' reg
	return $ eInstr1 ++ sInstr ++ eInstr2 ++ [Instr AAdd (aType Int) reg raxReg] ++ lInstr

genExpr' (EAdd expr1 Minus expr2) = do
	eInstr1 <- genExpr' expr2
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	eInstr2 <- genExpr' expr1
	releaseTmp typ' reg
	return $ eInstr1 ++ sInstr ++ eInstr2 ++ [Instr ASub (aType Int) reg raxReg] ++ lInstr

genExpr' (EMul expr1 Times expr2) = do
	eInstr1 <- genExpr' expr1
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	eInstr2 <- genExpr' expr2
	releaseTmp typ' reg
	return $ eInstr1 ++ sInstr ++ eInstr2 ++ [Instr AMul (aType Int) reg raxReg] ++ lInstr

genExpr' (EMul expr1 Div expr2) = do
	eInstr1 <- genExpr' expr2
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	eInstr2 <- genExpr' expr1
	releaseTmp typ' reg
	return $ eInstr1 ++ sInstr ++ eInstr2 ++ [Cdq, Instr ADiv (aType Int) reg raxReg] ++ lInstr

genExpr' e@(EAnd cond1 cond2) = do
	lTrue <- getNextLabel'
	lFalse <- getNextLabel'
	lAfter <- getNextLabel'
	eInstr <- genCond' e lTrue lFalse
	return $ eInstr ++ [LabelDecl lTrue] ++ [Instr AMov Byte (Con 1) raxReg, Goto lAfter, LabelDecl lFalse] ++ [Instr AMov Byte (Con 0) raxReg] ++ [LabelDecl lAfter]

genExpr' e@(EOr cond1 cond2) = do
	lTrue <- getNextLabel'
	lFalse <- getNextLabel'
	lAfter <- getNextLabel'
	eInstr <- genCond' e lTrue lFalse
	return $ eInstr ++ [LabelDecl lTrue] ++ [Instr AMov DWord (Con 1) raxReg, Goto lAfter, LabelDecl lFalse] ++ [Instr AMov DWord (Con 0) raxReg] ++ [LabelDecl lAfter]

genExpr' e@(ERel expr1 op expr2) = do
	lTrue <- getNextLabel'
	lFalse <- getNextLabel'
	lAfter <- getNextLabel'
	eInstr <- genCond' e lTrue lFalse
	return $ eInstr ++ [LabelDecl lTrue] ++ [Instr AMov DWord (Con 1) raxReg, Goto lAfter, LabelDecl lFalse] ++ [Instr AMov DWord (Con 0) raxReg] ++ [LabelDecl lAfter]

genExpr' (ENArr newArr) = do
	nInstr <- genNewArr newArr
	return nInstr

genExpr' (EArr arrAccess) = do
	(aInstrs, mInstr) <- genArrAccess' arrAccess
	return $ aInstrs ++ mInstr

genExpr' (EFld fieldAccess) = do
	(fAccess, fMov) <- genFldAccess' fieldAccess
	return $ fAccess ++ fMov

genExpr' (EMth methCall) = do
	(aInstrs, mInstr) <- genMethCall' methCall
	return $ aInstrs ++ mInstr
--genExpr' _ = do
--	return []

genNewArr :: MonadState LState m => NewArr -> m [Instr]
genNewArr (NewArr typ expr) = do
	eInstr <- genExpr' expr
	return $ eInstr ++ [Instr AMov DWord raxReg r12Reg,
						Instr AAdd DWord (Con 4) raxReg,
	 					Instr AMov DWord raxReg rdiReg,
	 					Call "malloc",
	 					Instr AMov DWord r12Reg (MemS raxReg 0)]

genAlloc :: MonadState LState m => Alloc -> m [Instr]
genAlloc (Alloc ident) = do
	classInfo <- getClassInfo ident
	mInstr <- forM (M.toList $ attrs classInfo) loadZero
	return $ [Instr AMov QWord (Con $ 8 + (cSize classInfo)) rdiReg, Call "malloc"] ++ mInstr
	where
		loadZero ((Ident "self") ,(ClassVar typ shift)) = do 
			return $ Instr AMov (aType typ) raxReg (MemS raxReg shift)
		loadZero (ident ,(ClassVar typ shift)) = do
			return $ Instr AMov (aType typ) (Con 0) (MemS raxReg shift)

placeObject :: MonadState LState m => m ([Instr])
placeObject = do
	S (_, _, _, cInfo, _, _, _, _) <- get
	if cInfo == CNone then return []
	else do
		restoreReg (Class $ name cInfo) r12Reg
		return [Instr AMov QWord rdiReg r12Reg]

clearUsedRegs :: MonadState LState m => m ([Instr], [Instr])
clearUsedRegs = do
	currentShift <- getCurrentShift
	usedRegs <- getUsedRegsNonSafe
	freeSafe <- getFreeSafeRegs
	setCurrentShift currentShift
	(_, store, restore) <- foldM moveToSafeLocation (freeSafe, [], []) usedRegs
	return (store, restore)


moveToSafeLocation :: MonadState LState m => ([Object], [Instr], [Instr]) -> ARegister -> m  ([Object], [Instr], [Instr])
moveToSafeLocation ([], store, restore) r = do
	shift <- addTmpVar $ typeSize $ rType r
	return ([], (Instr AMov (aType $ rType r) (reg r) (MemS rbpReg shift)):store, (Instr AMov (aType $ rType r) (MemS rbpReg shift) (reg r)):restore)

moveToSafeLocation ((f:fx), store, restore) r = do
	return (fx, (Instr AMov (aType $ rType r) (reg r) f):store, (Instr AMov (aType $ rType r) f (reg r)):restore)


callExpr :: MonadState LState m => ([Instr], [Object], Integer) -> (Expr, Type) -> m ([Instr], [Object], Integer)
callExpr (instr, [], i) (expr, typ) = do
	instr' <- genExpr' expr
	return (instr' ++ [Instr AMov (aType typ) raxReg (MemS rspReg (i * 8))] ++ instr, [], i + 1)
	
callExpr (instr, r:rx, i) (expr, typ) = do
	instr'  <- genExpr' expr
	return (instr' ++ [Instr AMov (aType typ) raxReg r] ++ instr,  rx, i)

callExprs [] _ _ = return []
callExprs ((expr, typ):ex) [] i = do
	instr  <- genExpr' expr
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	instrs  <- callExprs ex [] (i + 1)
	releaseTmp typ' reg
	return $ instr ++ sInstr ++ instrs ++ lInstr ++ [Instr AMov (aType typ) reg (MemS rspReg (i * 8))]

callExprs ((expr, typ):ex) (r:rx) i = do
	instr  <- genExpr' expr
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	instrs  <- callExprs ex rx i
	releaseTmp typ' reg
	return $ instr ++ sInstr ++ instrs ++ lInstr ++ [Instr AMov (aType typ) reg r]

storeTmp typ currentReg = do
	reg <- getFreeReg typ
	if reg /= emptyReg then
		return ([Instr AMov (aType typ) currentReg reg], [], reg, Void)
	else do
		(salvagedReg, typ2) <- getRegToSalvage typ
		shift <- addTmpVar $ typeSize typ2
		return ([Instr AMov (aType typ2) salvagedReg (MemS rbpReg shift),
				 Instr AMov (aType typ) currentReg salvagedReg],
				[Instr AMov (aType typ2) (MemS rbpReg shift) salvagedReg],
				salvagedReg, typ2)

releaseTmp typ reg = do
	delTmpVar $ typeSize typ
	if typ /= Void then
		restoreReg typ reg
	else
		freeReg reg

getPredefined "error" = return f_error
getPredefined "readInt" = return f_readInt
getPredefined "readString" = return f_readString
getPredefined "printInt" = return f_printInt
getPredefined "printString" = return f_printString
getPredefined "__add_str" = return f_add_string

f_error = [Instr AMov QWord (ConS "error_str") rdiReg,
		   Call "printf",
		   Instr AMov DWord (Con 60) raxReg,
		   Instr AMov DWord (Con 1) rdiReg,
		   Syscall]

f_printInt = [Instr AMov QWord rdiReg rsiReg,
			  Instr AMov QWord (ConS "print_schema") rdiReg,
			  Call "printf"]

f_printString = [Call "puts"]

f_readInt = [Call "readString",
			 Instr AMov QWord raxReg rdiReg,
			 Call "atoi"]

f_readString = [Instr ASub QWord (Con 24) rspReg,
				Instr AMov DWord (Con 2) rdiReg,
				Instr AMov QWord (Con 1) (MemS rspReg 0),
				Call "malloc",
				Instr ALea QWord (MemS rspReg (8)) rdiReg,
				Instr AMov QWord rspReg rsiReg,
				Instr AMov QWord (MemIO "rip" "stdin") rdxReg,
				Instr AMov QWord raxReg (MemS rspReg 8),
				Call "getline",
				Instr AMov QWord (MemS rspReg 0) raxReg,
				Instr AMov QWord (MemS rspReg 8) rdxReg,
				Instr AMov Byte (Con 0) (MemD rdxReg raxReg 1 (-2)),
				Instr AMov QWord (MemS rspReg 8) raxReg,
				Instr AAdd QWord (Con 24) rspReg]

f_add_string = [Instr AMov QWord rdiReg r13Reg,
				Instr AMov QWord rsiReg r12Reg,
				Call "strlen",
				Instr AMov QWord r12Reg rdiReg,
				Instr AMov QWord raxReg r14Reg,
				Call "strlen",
				Instr ALea QWord (MemD r14Reg raxReg 1 0) rdiReg,
				Call "malloc",
				Instr AMov QWord r14Reg rdxReg,
				Instr AMov QWord raxReg rbxReg,
				Instr AMov QWord r13Reg rsiReg,
				Instr AMov QWord raxReg rdiReg,
				Call "memcpy",
				Instr ALea QWord (MemD rbxReg r14Reg 1 0) rdiReg,
				Instr AMov QWord r12Reg rsiReg,
				Call "strcpy",
				Instr AMov QWord rbxReg raxReg]

genStrDecl' (strId, str) = do
	return $ AStr strId str

toEndLabel (Ident ident) = Label $ ".end_" ++ (id ident)

getClassIdent (Class ident) = ident
getFunArgTypes (Fun typ types) = types
