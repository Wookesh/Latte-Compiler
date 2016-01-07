{-# LANGUAGE FlexibleContexts #-}
module Compile where

import Data.Map as M
import Control.Monad.State
import LatteState
import AbsLatte
import Common
import Predefined

data AProgram = AProgram [ADef] deriving (Eq, Ord)

data ADef = AFun String [Instr] | AStr String String deriving (Eq, Ord)

data Instr = 
	  Instr AsOp AType Object Object
	| InstrS AsSOp AType Object
	| ARet
	| Cdq
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
	S (context, state, funInfo, label, regs, pResults, cStr) <- get
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
	S (context, state, funInfo, label, regs, pResults, cStr) <- get
	put $ S (context, state, funInfo, label + 1, regs, pResults, cStr)
	return $ ".L" ++ (show label)

getNextLabel' :: MonadState LState m => m Label
getNextLabel' = do
	S (context, state, funInfo, label, regs, pResults, cStr) <- get
	put $ S (context, state, funInfo, label + 1, regs, pResults, cStr)
	return $ Label $ ".L" ++ (show label)

showObj :: AType -> Object -> String
showObj _ (Con i) = "$" ++ (show i)
showObj _ (ConS s) = "$" ++ (id s)
showObj _ (Mem name shift) = (isShift shift) ++ "(%" ++ (id name) ++ ")" where
		isShift :: Integer -> String
		isShift shift = if shift /= 0 then 
			(show shift)
		else
			""
showObj Byte reg = "%" ++ (id $ r8 reg)
showObj DWord reg = "%" ++ (id $ r32 reg)
showObj QWord reg = "%" ++ (id $ r64 reg)
showObj Word reg = "%" ++ (id $ r64 reg)

jumpOp LTH = "jl"
jumpOp LE = "jle"
jumpOp GTH = "jg"
jumpOp GE = "jge"
jumpOp EQU = "je"
jumpOp NE = "jne"

toReg (VarInfo typ shift loc) = (Mem "rbp" shift)


getCharSize Int = return "l"
getCharSize Bool = return "b"
getCharSize _ = return "l"

genProg' (Program topDefs) = do
	strDefs <- getStrDefs
	sDecls <- forM (strDefs ++ predefinedString) genStrDecl'
	funDefs <- forM topDefs genTopDef'
	return (AProgram $ sDecls ++ funDefs)

genTopDef' (TopFun funDef) = do
	funDef' <- genFun' funDef
	return funDef'

makeProlog args addshift usedRegs = do
	(instr, _, _) <- foldM genArgInstr ([], 0, regCallOrder) args
	savedRegisters <- forM (Prelude.filter (\r -> elem r usedRegs) toSave) (\r -> return (InstrS Push QWord r))
	localVarShift <- getTotalShift
	return $ [InstrS Push QWord rbpReg,Instr AMov QWord rspReg rbpReg] ++ [Instr ASub QWord (Con $ (addshift + (toInteger $ ((length savedRegisters) `mod` 2) * 8) + addjustTo16 (-localVarShift))) rspReg] ++ savedRegisters ++ (reverse instr)

genArgInstr (instr, i, []) _ = return (instr, i, [])
genArgInstr (instr, i, (r:freeRegs)) (Arg _ ident) = do
	varInfo <- getVar ident
	return ((Instr AMov (aType $ vType varInfo) r (Mem "rbp" (shift varInfo)):instr), i, freeRegs)

makeEpilog args addshift usedRegs = do
	savedRegisters <- forM (Prelude.filter (\r -> elem r usedRegs) toSave) (\r -> return (InstrS Pop QWord r))
	localVarShift <- getTotalShift
	ident <- getFunFromContext
	return $ [LabelDecl $ toEndLabel ident] ++ (reverse savedRegisters) ++ [Instr AAdd QWord (Con $ (addshift + (toInteger $ ((length savedRegisters) `mod` 2) * 8) + addjustTo16 (-localVarShift))) rspReg] ++ [Instr AMov QWord rbpReg rspReg, InstrS Pop QWord rbpReg, ARet]

genFun' f@(FnDef typ i@(Ident ident) args block) = do
	ret <- runForFun i $ genFun f
	return ret

genFun f@(FnDef typ i@(Ident ident) args block) = do
		_ <- (foldM declParam (1) args)
		instrs <- getBody f
		addjustShift
		shift <- foldM countRspShift 0 instrs
		usedRegs <- foldM collectUsedSafeRegs [] instrs
		prolog <- makeProlog args (addjustTo16 shift) usedRegs
		epilog <- makeEpilog args (addjustTo16 shift) usedRegs
		return (AFun ident (prolog ++ instrs ++ epilog))
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

countRspShift prev (Instr asOp typ o1 (Mem "rsp" i)) = return $ max prev i
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
	lFalse <- getNextLabel'
	sInstr <- genStmt' stmt
	eInstr <- genCond' expr lTrue lFalse
	return $ [Goto lFalse, LabelDecl lTrue] ++ sInstr ++ [LabelDecl lFalse] ++ (init eInstr)

genStmt' (SExp expr) = do
	eInstr <- genExpr' expr
	return eInstr

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

-- Expresions

genCond' e@(EApp ident exprs) lTrue lFalse = do
	eInstr <- genExpr' e
	return $ eInstr ++ [Instr ACmp (aType Bool) (Con 0) raxReg, Jump NE lTrue, Goto lFalse]

genCond' (ERel expr1 op expr2) lTrue lFalse = do
	eInstr1 <- genExpr' expr1
	(sInstr, lInstr, reg, typ') <- storeTmp Int raxReg
	eInstr2 <- genExpr' expr2
	releaseTmp typ' reg
	return $ eInstr1 ++ sInstr ++ eInstr2 ++ [Instr ACmp (aType Int) reg raxReg]  ++ lInstr ++ [Jump (opposite op) lTrue, Goto lFalse]

genCond' (EVar ident) lTrue lFalse = do
	varInfo <- getVar ident
	return [(Instr ACmp (aType $ vType varInfo) (Con 0) (Mem "rbp" (shift varInfo))),(Jump NE lTrue), (Goto lFalse)]

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

genCond' _ _ _ = do
	return []

genExpr' (EVar ident) = do
	varInfo <- getVar ident
	return [(Instr AMov (aType $ vType varInfo) (Mem "rbp" (shift varInfo)) raxReg)]

genExpr' (ELitInt i) = do
	return [Instr AMov DWord (Con i) raxReg]

genExpr' ELitTrue = do
	return [Instr AMov Byte (Con 1) raxReg]

genExpr' ELitFalse = do
	return [Instr AMov Byte (Con 0) raxReg]

genExpr' (EApp i@(Ident ident) exprs) = do
	(store, restore) <- clearUsedRegs
	(Fun _ types) <- getFunType i
	(instr, _, _) <- foldM callExpr ([], regCallOrder, 0) $ zip exprs types
	return $ store ++ instr ++ [Call ident] ++ restore
	where
		clearUsedRegs = do
			currentShift <- getCurrentShift
			usedRegs <- getUsedRegsNonSafe
			freeSafe <- getFreeSafeRegs
			setCurrentShift currentShift
			(_, store, restore) <- foldM moveToSafeLocation (freeSafe, [], []) usedRegs
			return (store, restore)

		moveToSafeLocation ([], store, restore) r = do
			shift <- addTmpVar $ typeSize $ rType r
			return ([], (Instr AMov (aType $ rType r) (reg r) (Mem "rbp" shift)):store, (Instr AMov (aType $ rType r) (Mem "rbp" shift) (reg r)):restore)

		moveToSafeLocation ((f:fx), store, restore) r = do
			return (fx, (Instr AMov (aType $ rType r) (reg r) f):store, (Instr AMov (aType $ rType r) f (reg r)):restore)

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


genExpr' _ = do
	return []

callExpr (instr, [], i) (expr, typ) = do
	instr' <- genExpr' expr
	return (instr' ++ [Instr AMov (aType typ) raxReg (Mem "rsp" (i * 8))] ++ instr, [], i + 1)
	
callExpr (instr, r:rx, i) (expr, typ) = do
	instr'  <- genExpr' expr
	return (instr' ++ [Instr AMov (aType typ) raxReg r] ++ instr, rx, i)

storeTmp typ currentReg = do
	reg <- getFreeReg typ
	if reg /= emptyReg then
		return ([Instr AMov (aType typ) currentReg reg], [], reg, Void)
	else do
		(salvagedReg, typ2) <- getRegToSalvage typ
		shift <- addTmpVar $ typeSize typ2
		return ([Instr AMov (aType typ2) salvagedReg (Mem "rbp" shift),
				 Instr AMov (aType typ) currentReg salvagedReg],
				[Instr AMov (aType typ2) (Mem "rbp" shift) salvagedReg],
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

f_error = [Instr AMov QWord (ConS "error_str") rdiReg,
		   Call "printf",
		   Instr AMov DWord (Con 60) raxReg,
		   Instr AMov DWord (Con 1) rdiReg,
		   Syscall]

f_printInt = [Instr AMov QWord rdiReg rsiReg,
			  Instr AMov QWord (ConS "print_schema") rdiReg,
			  Call "printf"]

f_printString = [Instr AMov QWord rdiReg rsiReg,
				 Instr AMov QWord (ConS "scan_str_schema") rdiReg,
				 Call "printf"]
f_readInt = [Instr ALea QWord (Mem "rbp" (-4)) rsiReg,
			 Instr AMov QWord (ConS "scan_schema") rdiReg,
			 Call "scanf",
			 Instr AMov DWord (Mem "rbp" (-4)) raxReg]
f_readString = [Instr ALea QWord (Mem "rbp" (-8)) rsiReg,
				Instr AMov QWord (ConS "scan_str_schema") rdiReg,
				Call "scanf",
				Instr AMov DWord (Mem "rbp" (-8)) raxReg]

genStrDecl' (strId, str) = do
	return $ AStr strId str

toEndLabel (Ident ident) = Label $ ".end_" ++ (id ident)
