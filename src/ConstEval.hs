{-# LANGUAGE FlexibleContexts #-}
module ConstEval where

import AbsLatte
import Control.Monad.State
import LatteState
import Common
import TypeChecker

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
		newClassBlock <- runForClass ident $ evalConst classBlock
		return $ ClassDef ident newClassBlock

	evalConst (ClassDefE ident1 ident2 classBlock) = do
		newClassBlock <- runForClass ident1 $ evalConst classBlock
		return $ ClassDefE ident1 ident2 newClassBlock

instance ConstexprEvaluator FunDef where
	evalConst f@(FnDef typ ident args block) = do
		ret <- runForFun ident $ go f
		return ret
		where 
			go (FnDef typ ident args block) = do
				forM_ args declParam
				newBlock <- evalConst block
				return (FnDef typ ident args newBlock)
	 		
	 		declParam (Arg typ ident) = do
	 			declNoShift ident typ

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
		newStmts <- runForBlock $ forM stmts evalConst
		return $ Block newStmts

instance ConstexprEvaluator Stmt where

	evalConst (BStmt block) = do
		newBlock <- evalConst block
		return $ BStmt newBlock

	evalConst (Decl typ items) = do
		forM items $ declItem typ
		newItems <- forM items evalConst
		return $ Decl typ newItems

	evalConst (Ass ident expr) = do
		newExpr <- evalConst expr
		return $ Ass ident newExpr

	evalConst (Ret expr) = do
		newExpr <- evalConst expr
		return $ Ret newExpr

	evalConst (Cond ELitFalse stmt) = do
		return $ BStmt $ Block []

	evalConst (Cond ELitTrue stmt) = do
		return stmt

	evalConst (Cond expr stmt) = do
		newExpr <- evalConst expr
		case newExpr of
			ELitFalse -> return $ BStmt $ Block []
			ELitTrue -> do
				newStmt <- evalConst stmt
				return newStmt
			_ -> do
				newStmt <- evalConst stmt
				return $ Cond newExpr newStmt


	evalConst (CondElse expr stmt1 stmt2) = do
		newExpr <- evalConst expr
		case newExpr of 
			ELitFalse -> do 
				newStmt2 <- evalConst stmt2
				return newStmt2
			ELitTrue -> do
				newStmt1 <- evalConst stmt1
				return newStmt1
			_ -> do
				newStmt1 <- evalConst stmt1
				newStmt2 <- evalConst stmt2
				return $ CondElse newExpr newStmt1 newStmt2

	evalConst (While expr stmt) = do
		newExpr <- evalConst expr
		newStmt <- evalConst stmt
		return $ While newExpr newStmt

	evalConst (SExp expr) = do
		newExpr <- evalConst expr
		return $ SExp newExpr

	evalConst stmt = return stmt

instance ConstexprEvaluator Item where
	evalConst (Init ident expr) = do
		newExpr <- evalConst expr
		return $ Init ident newExpr

	evalConst item = return item

instance ConstexprEvaluator Expr where

	evalConst (EApp ident exprs) = do
		newExprs <- forM exprs evalConst
		return (EApp ident newExprs)

	evalConst (Not expr) = do
		newExpr <- evalConst expr
		case newExpr of
			ELitTrue -> return ELitFalse
			ELitFalse -> return ELitTrue
			Not expr -> return expr
			_ -> return $ Not newExpr

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
			(_, ELitInt n2) -> return $ ERel (ELitInt n2) (opposite op) newExpr1
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
			(_, _) -> do
				typ <- getType expr1
				if and [typ == Str, op == Plus] then
					return $ EApp (Ident "__add_str") [newExpr1,newExpr2]
				else 
					return $ EAdd newExpr1 op newExpr2
 

	evalConst (EAnd expr1 expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(ELitTrue, ELitTrue) -> return ELitTrue
			(ELitFalse, _) -> return ELitFalse
			(_, ELitFalse) -> return ELitFalse
			(ELitTrue, _) -> return newExpr2
			(_, ELitTrue) -> return newExpr1
			(_, _) -> return $ EAnd newExpr1 newExpr2

	evalConst (EOr expr1 expr2) = do
		newExpr1 <- evalConst expr1
		newExpr2 <- evalConst expr2
		case (newExpr1, newExpr2) of
			(_, ELitTrue) -> return ELitTrue
			(ELitTrue, _) -> return ELitTrue
			(ELitFalse, ELitFalse) -> return ELitFalse
			(_, ELitFalse) -> return newExpr1
			(ELitFalse, _) -> return newExpr2
			(_, _) -> return $ EOr newExpr1 newExpr2

	evalConst expr = return expr
