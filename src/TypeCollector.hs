{-# LANGUAGE FlexibleContexts #-}
module TypeCollector where

import Control.Monad.State
import Data.Map as M
import AbsLatte
import LatteState

class TypeCollector a where
	collectTypes :: MonadState LState m => a -> m ()

instance TypeCollector Program where
	collectTypes (Program topDefs) = do
		forM_ topDefs collectTypes

instance TypeCollector TopDef where
	collectTypes (TopFun funDef) = do
		collectTypes funDef

	collectTypes (ClassDef ident classBlock) = do
		addClass ident
		runForClassDecl ident $ collectTypes classBlock

	collectTypes (ClassDefE ident base classBlock) = do
		addClassE ident base
		runForClassDecl ident $ collectTypes classBlock

instance TypeCollector FunDef where
	collectTypes (FnDef typ ident args block) = do
		types <- getArgsTypes args
		defFun ident $ Fun typ types
		runForFun ident $ collectTypes block
		return ()

instance TypeCollector Arg where
	collectTypes (Arg typ ident) = do
		declAt 0 ident typ

instance TypeCollector ClassBlock where
	collectTypes (ClassBlock elems) = do
		forM_ elems collectTypes

instance TypeCollector ClassElem where
	collectTypes (ClassAtr typ ident) = do
		defAttr ident typ

	collectTypes (ClassFun funDef) = do
		collectTypes funDef

instance TypeCollector Block where
	collectTypes (Block stmts) = do
		forM_ stmts collectTypes

instance TypeCollector Stmt where

	collectTypes (BStmt block) = do
		collectTypes block

	collectTypes (Decl typ items) = do
		forM_ items collectTypes
		--forM_ items $ declSimple typ

	collectTypes (Ass ident expr) = do
		collectTypes expr

	collectTypes (Ret expr) = do
		collectTypes expr

	collectTypes (CondElse expr stmt1 stmt2) = do
		collectTypes expr
		collectTypes stmt1
		collectTypes stmt2

	collectTypes (Cond expr stmt) = do
		collectTypes expr
		collectTypes stmt

	collectTypes (While expr stmt) = do
		collectTypes expr
		collectTypes stmt

	collectTypes (SExp expr) = do
		collectTypes expr

	collectTypes _ = return ()

instance TypeCollector Expr where
	collectTypes (EApp ident exprs) = do
		forM_ exprs collectTypes

	collectTypes (EString string) = do
		addString string

	collectTypes (Neg expr) = do
		collectTypes expr

	collectTypes (Not expr) = do
		collectTypes expr

	collectTypes (EMul expr1 op expr2) = do
		collectTypes expr1
		collectTypes expr2

	collectTypes (EAdd expr1 op expr2) = do
		collectTypes expr1
		collectTypes expr2

	collectTypes (ERel expr1 op expr2) = do
		collectTypes expr1
		collectTypes expr2

	collectTypes (EAnd expr1 expr2) = do
		collectTypes expr1
		collectTypes expr2

	collectTypes (EOr expr1 expr2) = do
		collectTypes expr1
		collectTypes expr2

	collectTypes _ = return ()

instance TypeCollector Item where
	collectTypes (NoInit ident) = return ()
	collectTypes (Init ident expr) = do
		collectTypes expr

declSimple typ (NoInit ident) = do
	declAt 0 ident typ

declSimple typ (Init ident expr) = do
	declAt 0 ident typ

getArgsTypes args = do
	types <- foldM getArgType [] args 
	return $ reverse types

getArgType types (Arg typ ident) = do	
	return $ typ:types
