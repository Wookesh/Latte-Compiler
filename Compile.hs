{-# LANGUAGE FlexibleContexts #-}
module Compile where

import Data.Map as M
import Control.Monad.State
import LatteState
import AbsLatte


getVar ident = do
	S (context, state, funInfo, label) <- get
	case M.lookup ident $ variables funInfo  of
		Just varInfo -> return varInfo
		Nothing -> fail $ "there is still something TODO??"

getNextLabel :: MonadState LState m => m String
getNextLabel = do
	S (context, state, funInfo, label) <- get
	put $ S (context, state, funInfo, label + 1)
	return $ ".L" ++ (show label)

instance Show Object where
	show (Mem name shift) = (isShift shift) ++ "(%" ++ (id name) ++ ")" where
		isShift :: Integer -> String
		isShift shift = if shift /= 0 then 
			(show shift)
		else
			""
	show (Con i) = "$" ++ (show i)
	show (Reg name) = "%" ++ (id name)

emit str = lift $ putStr str

emitEndL :: MonadTrans t => t IO ()
emitEndL = lift $ putStr "\n"

emitT str = emit $ "\t" ++ str

goto :: MonadTrans t => String -> t IO ()
goto label = emitT $ "jmp " ++ label ++ "\n"
declareLabel label = emit $ (id label) ++ ":\n"

jump :: MonadTrans t => RelOp -> String -> t IO ()
jump op label = do
	emitT $ (jumpOp op) ++ " " ++ (id label) ++ "\n"

jumpOp LTH = "jl"
jumpOp LE = "jle"
jumpOp GTH = "jg"
jumpOp GE = "jge"
jumpOp EQU = "je"
jumpOp NE = "jne"

toReg (VarInfo typ shift) = (Mem "rbp" shift)


getCharSize Int = return "l"
getCharSize Bool = return "b"
getCharSize _ = return "l"

cmp (ELitInt i) (EVar ident) = do
	var <- getVar ident
	charSize <- getCharSize $ vType var
	emitT $ "cmp" ++ charSize ++ "\t$" ++ (show i) ++ ", " ++ (show $ Mem "rbp" $ shift var) ++ "\n"

push letter dst = do
	emitT $ "push" ++ (id letter) ++ "\t" ++ (show dst) ++ "\n"

pop letter dst = do
	emitT $ "pop" ++ (id letter) ++ "\t" ++ (show dst) ++ "\n"

mov letter src dst = do
	emitT $ "mov" ++ (id letter) ++ "\t" ++ (show src) ++ ", " ++ (show dst) ++ "\n"

add src dst = do
	emitT $ "addl\t" ++ (show src) ++ ", " ++ (show dst) ++ "\n"

sub src dst = do
	emitT $ "subl\t" ++ (show src) ++ ", " ++ (show dst) ++ "\n"

declareVar (NoInit ident) = do
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	mov charSize (Con 0) $ toReg varInfo

declareVar (Init ident (ELitInt i)) = do
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	mov charSize (Con i) $ toReg varInfo

declareVar (Init ident ELitFalse) = do
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	mov charSize (Con 0) $ toReg varInfo

declareVar (Init ident ELitTrue) = do
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	mov charSize (Con 1) $ toReg varInfo

declareVar (Init ident expr) = do
	genExpr expr
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	mov charSize (Reg "eax") $ toReg varInfo

genProg (Program topDefs) = do
	emitT $ ".global main\n"
	forM_ topDefs genTopDef
	return ()

genTopDef (TopFun funDef) = do
	genFun funDef

genFun (FnDef typ i@(Ident ident) args block) = do
	emit $ (id ident) ++ ":\n"
	push "q" $ Reg "rbp"
	mov "q" (Reg "rsp") (Reg "rbp")
	--makeProlog args
	runForFun i $ genBlock block
	--makeEpilog args
	pop "q" (Reg "rbp")
	emitT $ "ret\n"
	return ()

genBlock (Block stmts) = do
	forM_ stmts genStmt


genStmt (Decl typ items) = do
	forM_ items $ declareVar

genStmt (While expr stmt) = do
	labelTrue <- getNextLabel
	labelFalse <- getNextLabel
	goto labelFalse
	declareLabel labelTrue
	genStmt stmt
	declareLabel labelFalse
	genCond expr labelTrue labelFalse

genStmt (Cond expr stmt) = do
	labelTrue <- getNextLabel
	labelAfter <- getNextLabel
	genCond expr labelTrue labelAfter
	declareLabel labelTrue
	genStmt stmt
	declareLabel labelAfter

genStmt (CondElse expr stmt1 stmt2) = do
	labelTrue <- getNextLabel
	labelFalse <- getNextLabel
	labelAfter <- getNextLabel
	genCond expr labelTrue labelFalse
	declareLabel labelTrue
	genStmt stmt1
	goto labelAfter
	declareLabel labelFalse
	genStmt stmt2
	declareLabel labelAfter

genStmt (VRet) = do
	mov "l" (Con 0) (Reg "eax")

genStmt (Ret expr) = do
	genExpr expr

genStmt (Incr ident) = do
	varInfo <- getVar ident
	add (Con 1) $ toReg varInfo

genStmt (Decr ident) = do
	varInfo <- getVar ident
	sub (Con 1) $ toReg varInfo

genStmt (Ass ident expr) = do
	emitT $ "addl" ++ (show 1) ++ "\t-4(%rbp)\n"

genStmt _ = return ()

genCond (ERel expr1 op expr2) lTrue lFalse = do
	reg1 <- getFreeReg
	reg2 <- getFreeReg
	genExpr expr1
	genExpr expr2
	cmp expr1 expr2
	jump op lTrue
	goto lFalse
	freeReg reg1
	freeReg reg2

genCond (EOr cond1 cond2) lTrue lFalse = do
	midLabel <- getNextLabel
	genCond cond1 lTrue midLabel
	declareLabel midLabel
	genCond cond2 lTrue lFalse

genCond (EVar ident) lTrue lFalse = do
	cmp (ELitInt 0) (EVar ident)
	jump NE lTrue
	goto lFalse

genCond _ _ _ = return ()


genExpr (ELitInt i) = do
	mov "l" (Con i) (Reg "eax")

genExpr (ELitTrue) = do
	mov "b" (Con 1) (Reg "eax")

genExpr (ELitFalse) = do
	mov "b" (Con 0) (Reg "eax")

genExpr (EVar ident) = do
	varInfo <- getVar ident
	charSize <- getCharSize $ vType varInfo
	mov charSize (Mem "rbp" (shift varInfo)) (Reg "eax")
