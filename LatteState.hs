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
 	decl :: MonadState s m => Ident -> Type -> m ()
 	getVarType :: MonadState s m => Ident -> m Type
 	getFunRetType :: MonadState s m => Ident -> m Type
 	getFunType :: MonadState s m => Ident -> m Type
 	runInContext :: MonadState s m => Context -> m b -> m b
 	runForClass :: MonadState s m => Ident -> m b -> m b
 	runForFun :: MonadState s m => Ident -> m b -> m b
 	local :: MonadState s m => m b -> m b
 	getFunFromContext :: MonadState s m => m Ident

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

	decl ident typ = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		put $ S (context, ST (M.insert ident typ vEnv, fEnv, cEnv))

	getVarType ident = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident vEnv of
			(Just typ) -> do return typ
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."

	getFunRetType ident = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident fEnv of
			(Just (Fun typ types)) -> do return typ
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."

	getFunType ident = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident fEnv of
			(Just typ) -> do return typ
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."



	runInContext context fun = do
		S (context', store) <- get
		put $ S (context, store)
		t <- fun 
		S (_, store') <- get
		put $ S (context', store')
		return t

	getFunFromContext = do
		S (context, store) <- get
		case getFunIdent context of
			Just ident -> return ident
			Nothing -> fail $ "not in function"


	runForClass ident fun = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident cEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just (base, atrs, fEnv')) -> do
				put $ S (enterContext context (C ident None), ST (vEnv, fEnv', cEnv))
				t <- fun
				S (context', ST (vEnv', fEnv'', cEnv')) <- get
				put $ S (context, ST (vEnv', fEnv, M.insert ident (base, atrs, fEnv'') cEnv'))
				return t

	runForFun ident fun = do
		S (context, ST (vEnv, fEnv, cEnv)) <- get
		case M.lookup ident fEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just typ) -> do
				put $ S (enterContext context (F ident), ST (vEnv, fEnv, cEnv))
				t <- fun
				S (context', ST (vEnv', fEnv', cEnv')) <- get
				put $ S (context, ST (vEnv', fEnv, cEnv))
				return t

	local fun = do
		state <- get
		t <- fun
		_ <- get
		put $ state
		return t

-- Context --

enterContext (C ident None) context = C ident context
enterContext _ context = context

getFunIdent (F ident) = Just ident
getFunIdent (C ident context) = getFunIdent context
getFunIdent None = Nothing