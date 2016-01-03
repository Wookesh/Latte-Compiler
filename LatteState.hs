module LatteState where

import Data.Map as M
import Control.Monad.State
import AbsLatte
import Common


data Object = 
	  Reg String
	| Mem String Integer
	| Con Integer deriving (Ord, Eq)
data VarInfo = VarInfo { vType :: Type, shift :: Integer } deriving (Show, Eq, Ord)
data FunInfo = FunInfo { fType :: Type, nextVarShift :: Integer, variables :: VEnv} | FNone deriving (Eq, Ord, Show)
type VEnv = M.Map Ident VarInfo
type FEnv = M.Map Ident FunInfo
type CEnv = M.Map Ident (Ident, [Arg], FEnv)
type RegList = ([Object], [Object])

data Store = ST (VEnv, FEnv, CEnv) deriving (Eq, Ord, Show)

data Context = F Ident| C Ident Context | None deriving (Eq, Ord, Show)

data LState = S (Context, Store, FunInfo, Integer, RegList)

---------- Store

clearState :: LState
clearState = S (None, clearStore, FNone, 1, clearRegList)

clearStore :: Store
clearStore = ST (clearVEnv, clearFEnv, clearCEnv)

clearCEnv :: CEnv
clearCEnv = M.empty

clearVEnv :: VEnv
clearVEnv = M.empty

clearFEnv :: FEnv
clearFEnv = M.empty

clearRegList = ((Reg "eax"):(Reg "edx"):(Reg "ebx"):(Reg "ecx"):(Reg "esi"):(Reg "edi"):(Reg "r8d"):(Reg "r9d"):(Reg "r10d"):(Reg "r11d"):(Reg "r12d"):(Reg "r13d"):(Reg "r14d"):(Reg "r15d"):[],
				(Reg "rax"):(Reg "rdx"):(Reg "rbx"):(Reg "rcx"):(Reg "rsi"):(Reg "rdi"):(Reg "r8"):(Reg "r9"):(Reg "r10"):(Reg "r11"):(Reg "r12"):(Reg "r13"):(Reg "r14"):(Reg "r15"):[])

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
 	freeReg :: MonadState s m => Object -> m ()
 	getFreeReg32 :: MonadState s m => m Object
 	getFreeReg :: MonadState s m => m Object

instance StoreOperations LState where
	addClass ident classBlock = addClassE ident (Ident "") classBlock

	addClassE ident base classBlock = do
		S (context, ST (vEnv, fEnv, cEnv), funInfo, lastLabel, regs) <- get
		case M.member ident cEnv of
			True -> fail $ "Redefinition of class " ++ (show ident) ++ "."
			False -> do
				put $ S (context, ST (vEnv, fEnv, M.insert ident (base, [], clearFEnv) cEnv), funInfo, lastLabel, regs)

	defFun ident typ = do
		S (context, ST (vEnv, fEnv, cEnv), funInfo, lastLabel, regs) <- get
		case M.lookup ident fEnv of
			(Just typ1) -> fail $ "Redefinition of function " ++ (show ident) ++ "::" ++ (show typ1) ++ "."
			Nothing -> do
				put $ S (context, ST (vEnv, M.insert ident (FunInfo typ (-4) clearVEnv) fEnv, cEnv), funInfo, lastLabel, regs)

	decl ident typ = do
		S (context, ST (vEnv, fEnv, cEnv), (FunInfo fTyp totalShift vars), lastLabel, regs) <- get
		put $ S (context, ST (vEnv, fEnv, cEnv), (FunInfo fTyp (totalShift - (typeSize typ)) (M.insert ident (VarInfo typ totalShift) vars)), lastLabel, regs)

	getVarType ident = do
		S (context, ST (vEnv, fEnv, cEnv), funInfo, lastLabel, regs) <- get
		case M.lookup ident (variables funInfo) of
			(Just var) -> do return $ vType var
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."

	getFunRetType ident = do
		S (context, ST (vEnv, fEnv, cEnv), funInfo, lastLabel, regs) <- get
		case M.lookup ident fEnv of
			(Just funInfo) -> 
				let (Fun typ types) = fType funInfo in return $ typ
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."

	getFunType ident = do
		S (context, ST (vEnv, fEnv, cEnv), funInfo, lastLabel, regs) <- get
		case M.lookup ident fEnv of
			(Just funInfo) -> do return $ fType funInfo
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."



	runInContext context fun = do
		S (context', store, funInfo, lastLabel, regs) <- get
		put $ S (context, store, funInfo, lastLabel, regs)
		t <- fun 
		S (_, store', funInfo, lastLabel, regs) <- get
		put $ S (context', store', funInfo, lastLabel, regs)
		return t

	getFunFromContext = do
		S (context, store, funInfo, lastLabel, regs) <- get
		case getFunIdent context of
			Just ident -> return ident
			Nothing -> fail $ "not in function"


	runForClass ident fun = do
		S (context, ST (vEnv, fEnv, cEnv), fNone, lastLabel, regs) <- get
		case M.lookup ident cEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just (base, atrs, fEnv')) -> do
				put $ S (enterContext context (C ident None), ST (vEnv, fEnv', cEnv), fNone, lastLabel, regs)
				t <- fun
				S (context', ST (vEnv', fEnv'', cEnv'), fNone, lastLabel, regs) <- get
				put $ S (context, ST (vEnv', fEnv, M.insert ident (base, atrs, fEnv'') cEnv'), fNone, lastLabel, regs)
				return t

	runForFun ident fun = do
		S (context, ST (vEnv, fEnv, cEnv), _, lastLabel, regs) <- get
		case M.lookup ident fEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just funInfo) -> do
				put $ S (enterContext context (F ident), ST (vEnv, fEnv, cEnv), funInfo, lastLabel, regs)
				t <- fun
				S (context', ST (vEnv', fEnv', cEnv'), funInfo, lastLabel', regs) <- get
				put $ S (context, ST (vEnv', M.insert ident funInfo fEnv, cEnv), FNone, lastLabel', regs)
				return t

	local fun = do
		state <- get
		t <- fun
		_ <- get
		put $ state
		return t

	freeReg reg = do

	getFreeReg

-- Context --

enterContext (C ident None) context = C ident context
enterContext _ context = context

getFunIdent (F ident) = Just ident
getFunIdent (C ident context) = getFunIdent context
getFunIdent None = Nothing