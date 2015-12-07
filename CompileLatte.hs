{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CompileLatte where

import AbsLatte
import Control.Monad.State
import LatteState
import TypeCollector


compileProg prog = evalStateT (runCompiler prog) $ clearState

runCompiler prog = do
	collectTypes prog
	checkType prog

class ConstexprEvaluator a where
	evalConst :: MonadState LState m => a -> m a

class TypeChecker a where
	checkType :: MonadState LState m => a -> m ()
	checkType a = do return ()


---------- type checker
instance TypeChecker Program where
	checkType (Program topDefs) = do
		forM_ topDefs checkType

instance TypeChecker TopDef where
	checkType (TopFun funDef) = do
		checkType funDef

	checkType (ClassDef ident classBlock) = do
		checkType classBlock

	checkType (ClassDefE ident base classBlock) = do
		checkType classBlock

instance TypeChecker FunDef where
	 --checkType (FnDef typ ident args block) = do
	 --	checkType block

instance TypeChecker ClassBlock where
	checkType (ClassBlock elems) = do
		forM_ elems checkType

instance TypeChecker ClassElem where
	checkType (ClassFun funDef) = do
		return ()
