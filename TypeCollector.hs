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
		addClass ident classBlock
		runInContext (C ident None) $ collectTypes classBlock

	collectTypes (ClassDefE ident base classBlock) = do
		addClassE ident base classBlock
		runForClass ident $ collectTypes classBlock

instance TypeCollector FunDef where
	collectTypes (FnDef typ ident args block) = do
		types <- getArgsTypes args
		defFun ident $ Fun typ types
		return ()

instance TypeCollector ClassBlock where
	collectTypes (ClassBlock elems) = do
		forM_ elems collectTypes

instance TypeCollector ClassElem where
	collectTypes (ClassAtr typ ident) = do
		return ()
	collectTypes (ClassFun funDef) = do
		collectTypes funDef


getArgsTypes args = do
	types <- foldM getType [] args 
	return $ reverse types

getType types (Arg typ ident) = do	
	return $ typ:types
