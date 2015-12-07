{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CompileLatte where

import AbsLatte
import Data.Map as M
import Data.Set as S
import Control.Monad.State

type Reg = Int

type VEnv = M.Map Ident Reg
type FEnv = M.Map Ident (Type, [Arg])
type CEnv = M.Map Ident (Ident, [Arg], FEnv)

data Store = ST (VEnv, FEnv, CEnv) deriving (Eq, Ord, Show)

data Context = F Ident| C Ident Context| None deriving (Eq, Ord, Show)

data LState = S (Context, Store)

---------- Store

clearState :: LState
clearState = S (None, clearStore)

clearStore :: Store
clearStore = ST (clearVEnv, clearFEnv, clearCEnv)

clearCEnv :: CEnv
clearCEnv = M.empty

clearVEnv :: VEnv
clearVEnv = M.empty

clearFEnv :: FEnv
clearFEnv = M.empty

class StoreOperations s where
 	addClass :: MonadState s m => Ident -> ClassBlock -> m ()
 	addClassE :: MonadState s m => Ident -> Ident -> ClassBlock -> m ()
 	defFun :: MonadState s m => Ident -> Type -> [Arg] -> m ()
 	runInContext :: MonadState s m => Context -> m b -> m b


instance StoreOperations LState where
	addClass ident classBlock = addClassE ident (Ident "") classBlock

	addClassE ident base classBlock = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.member ident cEnv of
			True -> fail $ "Redefinition of class " ++ (show ident) ++ "."
			False -> do
				put $ S (context, ST (vEnv, fEnv, M.insert ident (base, [], clearFEnv) cEnv))

	defFun ident typ args = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident fEnv of
			(Just args) -> fail $ "Redefinition of function " ++ (show ident) ++ " " ++ (show args) ++ "."
			Nothing -> do
				put $ S (context, ST (vEnv, M.insert ident (typ, args) fEnv, cEnv))

	runInContext context fun = do
		S (context', store) <- get
		put $ S (context, store)
		t <- fun 
		S (_, store') <- get
		put $ S (context', store')
		return t

compileProg prog = evalStateT (runCompiler prog) $ clearState

runCompiler prog = do
	collectTypes prog
	checkType prog

class ConstexprEvaluator a where
	evalConst :: MonadState LState m => a -> m a

class TypeChecker a where
	collectTypes ::MonadState LState m => a -> m ()
	collectTypes a = do return ()
	checkType :: MonadState LState m => a -> m ()
	checkType a = do return ()
---------- type checker
instance TypeChecker Program where
	collectTypes (Program topDefs) = do
		forM_ topDefs collectTypes

	checkType (Program topDefs) = do
		forM_ topDefs checkType

instance TypeChecker TopDef where
	collectTypes (TopFun funDef) = do
		collectTypes funDef

	collectTypes (ClassDef ident classBlock) = do
		addClass ident classBlock
		runInContext (C ident None) $ collectTypes classBlock

	collectTypes (ClassDefE ident base classBlock) = do
		addClassE ident base classBlock
		runInContext (C ident None) $ collectTypes classBlock

		
	checkType (TopFun funDef) = do
		checkType funDef

	checkType (ClassDef ident classBlock) = do
		checkType classBlock

	checkType (ClassDefE ident base classBlock) = do
		checkType classBlock

instance TypeChecker FunDef where
	collectTypes (FnDef typ ident args block) = do
		defFun ident typ args

	-- checkType (FnDef typ ident args block) = do
	-- 	checkType block

instance TypeChecker ClassBlock where
	collectTypes (ClassBlock elems) = do
		return ()

	checkType (ClassBlock elems) = do
		forM_ elems checkType

instance TypeChecker ClassElem where
	collectTypes (ClassAtr typ ident) = do
		return ()
