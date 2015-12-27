{-# LANGUAGE FlexibleContexts #-}
module ConstEval where

import AbsLatte
import Control.Monad.State
import LatteState

class ConstexprEvaluator a where
	evalConst :: MonadState LState m => a -> m a


instance ConstexprEvaluator Program where
	evalConst (Program topDefs) = do
		newTopDefs <- forM topDefs evalConst
		return $ Program newTopDefs

instance ConstexprEvaluator TopDef where
	evalConst (TopFun funDef) = do
		newFunDef <- evalConst funDef
		return $ TopFun newFunDef

	evalConst (ClassDef ident classBlock) = do
		newClassBlock <- evalConst classBlock
		return $ ClassDef ident newClassBlock

	evalConst (ClassDefE ident1 ident2 classBlock) = do
		newClassBlock <- evalConst classBlock
		return $ ClassDefE ident1 ident2 newClassBlock

instance ConstexprEvaluator FunDef where
	evalConst (FnDef typ ident arg block) = do
		newBlock <- runForFun ident $ evalConst block
		return $ FnDef typ ident arg newBlock

instance ConstexprEvaluator ClassBlock where
	evalConst (ClassBlock classElems) = do
		newClassElems <- forM classElems evalConst
		return $ ClassBlock newClassElems

instance ConstexprEvaluator ClassElem where
	evalConst (ClassFun funDef) = do
		newFunDef <- evalConst funDef
		return $ ClassFun newFunDef

	evalConst classElem = do
		return classElem

instance ConstexprEvaluator Block where
	evalConst (Block stmts) = do
		newStmts <- forM stmts evalConst
		return $ Block newStmts

instance ConstexprEvaluator Stmt where

	evalConst (BStmt block) = do
		newBlock <- evalConst block
		return $ BStmt newBlock

	evalConst (Decl typ items) = do
		newItems <- forM items evalConst
		return $ Decl typ newItems

	evalConst (Ass ident expr) = do
		newExpr <- evalConst expr
		return $ Ass ident newExpr

	evalConst (Ret expr) = do
		newExpr <- evalConst expr
		return $ Ret newExpr

	evalConst (Cond expr stmt) = do
		newExpr <- evalConst expr
		newStmt <- evalConst stmt
		return $ Cond newExpr newStmt

	evalConst (CondElse expr stmt1 stmt2) = do
		newExpr <- evalConst expr
		newStmt1 <- evalConst stmt1
		newStmt2 <- evalConst stmt2
		return $ CondElse newExpr newStmt1 newStmt2

	evalConst (While expr stmt) = do
		newExpr <- evalConst expr
		newStmt <- evalConst stmt
		return $ While newExpr newStmt

	evalConst stmt = return stmt

instance ConstexprEvaluator Item where
	evalConst (Init ident expr) = do
		newExpr <- evalConst expr
		return $ Init ident newExpr

	evalConst item = return item

instance ConstexprEvaluator Expr where

	evalConst (Not expr) = do
		newExpr <- evalConst expr
		case newExpr of
			ELitTrue -> return ELitFalse
			ELitFalse -> return ELitTrue
			_ -> return $ Neg newExpr

	evalConst (Neg expr) = do
		newExpr <- evalConst expr
		case newExpr of
			ELitInt n -> return $ ELitInt (-n)
			_ -> return $ Neg newExpr

	evalConst (ERel expr1 op expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(ELitInt n1, ELitInt n2) -> do
				case op of
					LTH -> if n1 < n2 then return ELitTrue else return ELitFalse
					LE -> if n1 <= n2 then return ELitTrue else return ELitFalse
					GTH -> if n1 > n2 then return ELitTrue else return ELitFalse
					GE -> if n1 >= n2 then return ELitTrue else return ELitFalse
					EQU -> if n1 == n2 then return ELitTrue else return ELitFalse
					NE -> if n1 /= n2 then return ELitTrue else return ELitFalse
			(_, _) -> return $ ERel newExpr1 op newExpr2

	evalConst (EMul expr1 op expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(ELitInt n1, ELitInt n2) -> do
				case op of
					Times -> return $ ELitInt (n1 * n2)
					Div -> return $ ELitInt (n1 `div` n2) 
					Mod -> return $ ELitInt (n1 `mod` n2)
			(_, _) -> return $ EMul newExpr1 op newExpr2


	evalConst (EAdd expr1 op expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(ELitInt n1, ELitInt n2) -> do
				case op of
					Plus -> return $ ELitInt (n1 + n2)
					Minus -> return $ ELitInt (n1 - n2)
			(_, _) -> return $ EAdd newExpr1 op newExpr2
 

	evalConst (EAnd expr1 expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(ELitTrue, ELitTrue) -> return ELitTrue
			(_, _) -> return $ EOr newExpr1 newExpr2

	evalConst (EOr expr1 expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(_, ELitTrue) -> return ELitTrue
			(ELitTrue, _) -> return ELitTrue
			(_, _) -> return $ EOr newExpr1 newExpr2

	evalConst expr = return expr
