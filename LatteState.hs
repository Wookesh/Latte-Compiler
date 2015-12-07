module LatteState where

import Data.Map as M
import Control.Monad.State
import AbsLatte


type VEnv = M.Map Ident Type
type FEnv = M.Map Ident Type
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
 	defFun :: MonadState s m => Ident -> Type -> m ()
 	runInContext :: MonadState s m => Context -> m b -> m b
 	runForClass :: MonadState s m => Ident -> m b -> m b

instance StoreOperations LState where
	addClass ident classBlock = addClassE ident (Ident "") classBlock

	addClassE ident base classBlock = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.member ident cEnv of
			True -> fail $ "Redefinition of class " ++ (show ident) ++ "."
			False -> do
				put $ S (context, ST (vEnv, fEnv, M.insert ident (base, [], clearFEnv) cEnv))

	defFun ident typ = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident fEnv of
			(Just typ1) -> fail $ "Redefinition of function " ++ (show ident) ++ "::" ++ (show typ1) ++ "."
			Nothing -> do
				put $ S (context, ST (vEnv, M.insert ident typ fEnv, cEnv))

	runInContext context fun = do
		S (context', store) <- get
		put $ S (context, store)
		t <- fun 
		S (_, store') <- get
		put $ S (context', store')
		return t

	runForClass ident fun = do
		S (context', ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident cEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just (base, atrs, fEnv')) -> do
				put $ S (context', ST (vEnv, fEnv', cEnv))
				t <- fun
				S (context', ST (vEnv', fEnv'', cEnv')) <- get
				put $ S (context', ST (vEnv', fEnv, M.insert ident (base, atrs, fEnv'') cEnv'))
				return t
