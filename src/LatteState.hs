module LatteState where

import Data.Map as M
import Data.Tuple as T
import Control.Monad.State
import AbsLatte
import Common


data Object = 
	  Reg {r64 :: String, r32 :: String, r8 :: String}
	| MemIO String String
	| MemD Object Object Integer Integer -- MemD base shift multiplyier shift2
	| MemS Object Integer
	| Con Integer
	| ConS String
	| Dummy deriving (Ord, Eq)
data VarInfo = VarInfo { vType :: Type, shift :: Integer, location :: Integer } | ClassVar {vType :: Type, shift :: Integer} deriving (Show, Eq, Ord)
data FunInfo = FunInfo { fType :: Type, nextVarShift :: Integer, totalShift :: Integer, variables :: [VEnv]} | FNone deriving (Eq, Ord, Show)
data ClassInfo = ClassInfo {name :: Ident, derives :: Ident, attrs :: VEnv, methods :: FEnv, cSize :: Integer} | CNone deriving (Show, Eq, Ord)
type VEnv = M.Map Ident VarInfo
type FEnv = M.Map Ident FunInfo
type CEnv = M.Map Ident ClassInfo
data PartialResults = PR { inUse :: M.Map Integer (Object, Type), nextFree :: Integer} deriving (Eq, Ord)
data ARegister = R { reg :: Object, rType :: Type} deriving (Eq, Ord)
data RegList = Regs [ARegister] deriving (Eq, Ord)

data Store = ST (FEnv, CEnv) deriving (Eq, Ord, Show)

data Context = F Ident| C Ident Context | None deriving (Eq, Ord, Show)

data StringData = SD {allString :: M.Map String String, sId :: Integer}

data LState = S (Context, Store, FunInfo, ClassInfo, Integer, RegList, PartialResults, StringData)

---------- Store

clearPartialResults :: PartialResults
clearPartialResults = PR M.empty 1

clearState :: LState
clearState = S (None, clearStore, FNone, CNone, 1, clearRegList, clearPartialResults, SD (M.empty) 1)

clearStore :: Store
clearStore = ST (clearFEnv, clearCEnv)

clearCEnv :: CEnv
clearCEnv = M.empty

clearVEnv :: VEnv
clearVEnv = M.empty

clearFEnv :: FEnv
clearFEnv = M.empty

emptyReg = Reg "" "" ""
rbpReg = Reg "rbp" "ebp" ""
rspReg = Reg "rsp" "esp" ""
raxReg = Reg "rax" "eax" "al"
rbxReg = Reg "rbx" "ebx" "bl"
rcxReg = Reg "rcx" "ecx" "cl"
rdxReg = Reg "rdx" "edx" "dl"
rsiReg = Reg "rsi" "esi" "sil"
rdiReg = Reg "rdi" "edi" "dil"
r8Reg = Reg "r8" "r8d" "r8b"
r9Reg = Reg "r9" "r9d" "r9b"
r10Reg = Reg "r10" "r10d" "r10b"
r11Reg = Reg "r11" "r11d" "r11b"
r12Reg = Reg "r12" "r12d" "r12b"
r13Reg = Reg "r13" "r13d" "r13b"
r14Reg = Reg "r14" "r14d" "r14b"
r15Reg = Reg "r15" "r15d" "r15b"

toSave = [rbxReg, r12Reg, r13Reg, r14Reg, r15Reg]

regCallOrder = [rdiReg, rsiReg, rdxReg, rcxReg, r8Reg, r9Reg]

clearRegList = Regs [R rbxReg Void, R rcxReg Void,	R rdxReg Void,	R rsiReg Void,
					 R rdiReg Void, R r8Reg Void,	R r9Reg Void,	R r10Reg Void,
					 R r11Reg Void, R r12Reg Void,	R r13Reg Void,	R r14Reg Void,
					 R r15Reg Void]

class StoreOperations s where
 	addClass :: MonadState s m => Ident -> m ()
 	addClassE :: MonadState s m => Ident -> Ident -> m ()
 	addClassE' :: MonadState s m => Ident -> Ident -> FEnv  -> VEnv -> Integer-> m ()
 	defAttr :: MonadState s m => Ident -> Type -> m ()
 	defFun :: MonadState s m => Ident -> Type -> m ()
 	decl :: MonadState s m => Ident -> Type -> m ()
 	declNoShift :: MonadState s m => Ident -> Type -> m ()
 	declAt :: MonadState s m => Integer -> Ident -> Type -> m ()
 	declClassVars :: MonadState s m => Ident -> m ()
 	getVarType :: MonadState s m => Ident -> m Type
 	getFieldType :: MonadState s m => Type -> Ident -> m Type
 	getFunRetType :: MonadState s m => Ident -> m Type
 	getFunType :: MonadState s m => Ident -> m Type
 	getMethod :: MonadState s m => Ident -> Ident -> m FunInfo
 	runInContext :: MonadState s m => Context -> m b -> m b
 	runForClass :: MonadState s m => Ident -> m b -> m b
 	runForClassDecl :: MonadState s m => Ident -> m b -> m b
 	runForBlock :: MonadState s m => m b -> m b
 	runForFun :: MonadState s m => Ident -> m b -> m b
 	local :: MonadState s m => m b -> m b
 	getFunFromContext :: MonadState s m => m Ident
 	freeReg :: MonadState s m => Object -> m ()
 	getFreeReg :: MonadState s m => Type -> m Object
 	getFreeSafeRegs :: MonadState s m => m [Object]
 	getFreeReg' :: MonadState s m => Type -> [ARegister] -> m (Object, [ARegister])
 	getUsedRegs :: MonadState s m => m [ARegister]
 	getResultLoc :: MonadState s m => Integer -> m (Object, Type)
	setResultLoc :: MonadState s m => Integer -> (Object, Type) -> m ()
	newResultLoc :: MonadState s m => (Object, Type) -> m Integer
	getTotalShift :: MonadState s m => m Integer
	addjustShift :: MonadState s m => m ()
	getStrDefs :: MonadState s m => m [(String, String)]
	addString :: MonadState s m => String -> m ()
	getStringId :: MonadState s m => String -> m String
	getRegToSalvage :: MonadState s m => Type -> m (Object, Type)
	restoreReg :: MonadState s m =>  Type -> Object -> m ()
	addTmpVar :: MonadState s m => Integer -> m Integer
	delTmpVar :: MonadState s m => Integer -> m ()
	getCurrentShift :: MonadState s m => m Integer
	setCurrentShift :: MonadState s m => Integer -> m ()
	getUsedRegsNonSafe :: MonadState s m => m [ARegister]
	getClassInfo :: MonadState s m => Ident -> m ClassInfo
	addOwnerObjectToArgs :: MonadState s m => [Arg] -> m [Arg]

instance StoreOperations LState where
	addClass ident = addClassE' ident (Ident "") clearFEnv clearVEnv 0

	addClassE ident base = do
		S (context, ST (fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup base cEnv of
			Nothing -> fail $ "Base class " ++ (show ident) ++ " is not defined."
			(Just cInfo) -> do 
				addClassE' ident base (methods cInfo) (attrs cInfo) (cSize cInfo)
				return ()

	addClassE' ident base mEnv attrs cSize = do
		S (context, ST (fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.member ident cEnv of
			True -> fail $ "Redefinition of class " ++ (show ident) ++ "."
			False -> do
				put $ S (context, ST (fEnv, M.insert ident (ClassInfo ident base (M.insert (Ident "self") (ClassVar (Class ident) 0) attrs) mEnv cSize) cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr)


	defAttr ident typ = do
		S (context, store, funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.member ident (attrs cInfo) of
			True -> fail $ "Redefinition of attribute " ++ (show ident) ++ "."
			False -> do
				put $ S (context, store, funInfo, (ClassInfo (name cInfo) (derives cInfo) (M.insert ident (ClassVar typ ((typeSize typ) + (cSize cInfo))) (attrs cInfo)) (methods cInfo) ((cSize cInfo) + (typeSize typ))), lastLabel, regs, pResults, cStr)


	defFun ident typ@(Fun typ' types') = do
		S (context, ST (fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident fEnv of
			(Just typ1) -> fail $ "Redefinition of function " ++ (show ident) ++ "::" ++ (show typ1) ++ "."
			Nothing -> do
				if cInfo /= CNone then do
					put $ S (context, ST (M.insert ident (FunInfo (Fun typ' ((Class (name cInfo)):types')) 0 0 [clearVEnv]) fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr)
				else do
					put $ S (context, ST (M.insert ident (FunInfo typ 0 0 [clearVEnv]) fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr)

	-- modifies local shift
	decl ident typ = do
		S (context, store, (FunInfo fTyp shift maxShift (vars:vEnvs)), cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident vars of
			Nothing -> do put $ S (context, store, (FunInfo fTyp (shift - (typeSize typ)) (min maxShift (shift - (typeSize typ))) ((M.insert ident (VarInfo typ (shift - (typeSize typ)) 0) vars):vEnvs)), cInfo, lastLabel, regs, pResults, cStr)
			_ -> fail $ "Redefinition of " ++ (show ident) ++ "\n"

	-- no shift modification, usefull outsize Compile.hs
	declAt pos ident typ = do
		S (context, store, (FunInfo fTyp shift maxShift (vars:vEnvs)), cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident vars of
			Nothing -> do put $ S (context, store, (FunInfo fTyp shift maxShift  ((M.insert ident (VarInfo typ pos 0) vars):vEnvs)), cInfo, lastLabel, regs, pResults, cStr)
			(Just var) -> fail $ "redefinition of " ++ (show ident) ++ " defined as " ++ (show var) ++  "\n"

	declNoShift ident typ = declAt 0 ident typ

	declClassVars ident = do
		S (context, store, (FunInfo fTyp shift maxShift (vars:vEnvs)), cInfo, lastLabel, regs, pResults, cStr) <- get
		case cInfo of
			CNone -> return ()
			classInfo -> do
				forM_ (M.toList $ attrs classInfo) (\(k, a) -> declNoShift k $ vType a)

	getVarType ident = do
		S (context, store, funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		varType <- getVarType' ident $ variables funInfo
		return varType
		where
			getVarType' _ [] = fail $ "Variable " ++ (show ident) ++ "is not defined."
			getVarType' ident (vEnv:vx) = do
				case M.lookup ident vEnv of
					(Just var) -> return $ vType var
					Nothing -> do
						fromLower <- getVarType' ident vx
						return fromLower

	getFieldType (Class ident) ident' = do
		S (context, ST (_, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident cEnv of
			(Just classInfo) -> do
				case M.lookup ident' (attrs classInfo) of
					(Just varInfo) -> return $ vType varInfo
					Nothing -> fail $ "Class " ++ (show ident) ++ " does not have attribute " ++ (show ident')
			Nothing -> fail $ "Class " ++ (show ident) ++ " is not defined."

	getFieldType _ ident = fail $ "Object does not have attribute " ++ (show ident)
		
	getFunRetType ident = do
		S (context, ST (fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident fEnv of
			(Just funInfo) -> 
				let (Fun typ types) = fType funInfo in return $ typ
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."

	getFunType ident = do
		S (context, ST (fEnv, cEnv), funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident fEnv of
			(Just funInfo) -> do return $ fType funInfo
			Nothing -> fail $ "Variable " ++ (show ident) ++ "is not defined."

	getMethod cIdent mIdent = do
		classInfo <- getClassInfo cIdent
		case M.lookup mIdent $ methods classInfo of
			Nothing -> fail $ "No method " ++ (show mIdent) ++ "defined for " ++ (show cIdent)
			(Just method) -> return method

	runInContext context fun = do
		S (context', store, funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		put $ S (context, store, funInfo, cInfo, lastLabel, regs, pResults, cStr)
		t <- fun 
		S (_, store', funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		put $ S (context', store', funInfo, cInfo, lastLabel, regs, pResults, cStr)
		return t

	getFunFromContext = do
		S (context, store, funInfo, cInfo, lastLabel, regs, pResults, cStr) <- get
		case getFunIdent context of
			Just ident -> return ident
			Nothing -> fail $ "not in function"

	runForClassDecl ident fun  = do
		S (context, ST (fEnv, cEnv), fNone, cNone, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident cEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just cInfo@(ClassInfo name base atrs fEnv' size)) -> do
				put $ S (enterContext context (C ident None), ST (fEnv', cEnv), fNone, cInfo, lastLabel, regs, pResults, cStr)
				t <- fun
				S (context', ST (fEnv'', cEnv'), fNone, cInfo', lastLabel, regs, pResults, cStr) <- get
				put $ S (context, ST (fEnv, M.insert ident (ClassInfo name base (attrs cInfo') fEnv'' (cSize cInfo')) cEnv'), fNone, CNone, lastLabel, regs, pResults, cStr)
				return t

	runForClass ident fun  = do
		S (context, ST (fEnv, cEnv), fNone, cNone, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident cEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " not found."
			(Just cInfo@(ClassInfo name base atrs fEnv' size)) -> do
				put $ S (enterContext context (C ident None), ST (M.union fEnv fEnv', cEnv), fNone, cInfo, lastLabel, regs, pResults, cStr)
				t <- fun
				S (context', soterą, fNoneą, cInfo', lastLabel, regsą, pResultsą, cStr) <- get
				put $ S (context, ST (fEnv, cEnv), fNone, CNone, lastLabel, regs, pResults, cStr)
				return t

	runForFun ident fun = do
		S (context, ST (fEnv, cEnv), _, cInfo, lastLabel, regs, pResults, cStr) <- get
		case M.lookup ident fEnv of
			Nothing -> fail $ "Function " ++ (show ident) ++ " not found."
			(Just funInfo) -> do
				put $ S (enterContext context (F ident), ST (fEnv, cEnv), funInfoWithEnv funInfo [if cInfo /= CNone then (attrs cInfo) else clearVEnv], cInfo, lastLabel, regs, pResults, cStr)
				t <- fun
				S (context', ST (fEnv', cEnv'), funInfo, cInfo, lastLabel', regs, pResults, cStr) <- get
				put $ S (context, ST (M.insert ident (funInfoWithEnv funInfo [clearVEnv]) fEnv, cEnv), FNone, cInfo, lastLabel', regs, pResults, cStr)
				return t

	runForBlock fun = do
		S (context, store, (FunInfo typ shift maxShift vEnvs), cInfo, lastLabel, regs, pResults, cStr) <- get
		put $ S (context, store,  FunInfo typ shift maxShift (clearVEnv:vEnvs), cInfo, lastLabel, regs, pResults, cStr)
		t <- fun
		S (context, store, (FunInfo typ shift' maxShift' vEnvs), cInfo, lastLabel, regs, pResults, cStr) <- get
		put $ S (context, store, (FunInfo typ shift (min maxShift maxShift') (tail vEnvs)), cInfo, lastLabel, regs, pResults, cStr)
		return t
		
	local fun = do
		state <- get
		t <- fun
		_ <- get
		put $ state
		return t

	freeReg r@(Reg r1 r2 r3) = do
		S (context, store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		regs' <- freeReg' r regs
		put $ S (context, store, fInfo, cInfo, lastLabel, Regs regs', pResults, cStr)
		where
			freeReg' r1 [] = fail $ "No such register"
			freeReg' r (r':rx) = do
				if r == (reg r') then
					return $ (R r Void):rx
				else do
					nRx <- freeReg' r rx
					return (r':nRx)

	freeReg _ = do return ()

	getFreeReg typ = do
		S (context, store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		(r, regs') <- getFreeReg' typ regs
		put $ S (context, store, fInfo, cInfo, lastLabel, Regs regs', pResults, cStr)
		return r

	getFreeReg' typ [] = return (emptyReg, [])
	getFreeReg' typ (r:rx) = do
		if (rType r) == Void then
			return (reg r, (R (reg r) typ):rx)
		else do
			(r', rx') <- getFreeReg' typ rx
			return (r', r:rx')

	getRegToSalvage typ = do
		S (context, store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		put $ S (context, store, fInfo, cInfo, lastLabel, Regs (getSalvagedReg regs), pResults, cStr)
		return $ (reg $ head regs, rType $ head regs)
		where
			getSalvagedReg :: [ARegister] -> [ARegister]
			getSalvagedReg (r:rx) = rx ++ [R (reg r) typ]
			getSalvagedReg [] = []

	restoreReg typ r = do
		S (context, store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		put $ S (context, store, fInfo, cInfo, lastLabel, Regs (restoreReg' typ r regs), pResults, cStr)
		where
			restoreReg' typ r1 (r2:rx) = if r1 == reg r2 then (R r typ):rx
											else r2:(restoreReg' typ r1 rx)
			restoreReg' _ _ [] = []

	getUsedRegs = do
		S (context, store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		return $ Prelude.filter (\r -> (rType r) /= Void) regs

	getUsedRegsNonSafe = do
		S (context,store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		return $ Prelude.filter (\r -> and [(rType r) /= Void, not $ elem (reg r) toSave]) regs

	getFreeSafeRegs = do
		S (context, store, fInfo, cInfo, lastLabel, Regs regs, pResults, cStr) <- get
		return $ Prelude.map (reg) $ Prelude.filter (\r -> and [(rType r) == Void, elem (reg r) toSave]) regs


	getResultLoc rId = do
		S (context, store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		case M.lookup rId $ inUse results of
			Nothing -> fail $ "No result " ++ (show rId) ++ " registered"
			(Just result) -> return result

	setResultLoc rId (object, typ) = do
		S (context,store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context, store, fInfo, cInfo, lastLabel, regs, PR (M.insert rId (object, typ) (inUse results)) (nextFree results), cStr)

	newResultLoc (object, typ) = do
		S (context,store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context, store, fInfo, cInfo, lastLabel, regs, PR (M.insert (nextFree results) (object, typ) (inUse results)) ((nextFree results) + 1), cStr)
		return $ nextFree results

	getTotalShift = do
		S (context, store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		return $ totalShift fInfo

	getCurrentShift = do
		S (context, store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		return $ nextVarShift fInfo

	addjustShift = do
		S (context, store, (FunInfo typ shift maxShift vars), cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context,store, (FunInfo typ (-(addjustTo16 (-shift))) (-(addjustTo16 (-maxShift))) vars), cInfo, lastLabel, regs, results, cStr)

	getStrDefs = do
		S (context, store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		return $ Prelude.map T.swap $ M.toList $ allString cStr

	addString string = do
		S (context, store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context, store, fInfo, cInfo, lastLabel, regs, results, SD (M.insert string (".S" ++ (show $ sId cStr)) $ allString cStr) (1 + (sId cStr)))

	getStringId strId = do
		S (context, store, fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		case M.lookup strId $ allString cStr of
			Nothing -> fail $ "String " ++ (show strId) ++ " is not defined"
			(Just str) -> return str

	addTmpVar size = do
		S (context, store, (FunInfo typ currentShift maxShift vars), cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context, store, (FunInfo typ (currentShift - size) maxShift vars), cInfo, lastLabel, regs, results, cStr)
		return (currentShift - size)

	delTmpVar size = do
		S (context, store, (FunInfo typ currentShift maxShift vars), cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context, store, (FunInfo typ (currentShift + size) maxShift vars), cInfo, lastLabel, regs, results, cStr)

	setCurrentShift shift = do
		S (context, store, (FunInfo typ currentShift maxShift vars), cInfo, lastLabel, regs, results, cStr) <- get
		put $ S (context, store, (FunInfo typ shift maxShift vars), cInfo, lastLabel, regs, results, cStr)

	getClassInfo ident = do
		S (context, ST (fEnv, cEnv), fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		case M.lookup ident cEnv of
			Nothing -> fail $ "Class " ++ (show ident) ++ " is not defined."
			(Just cInfo) -> return cInfo

	addOwnerObjectToArgs args = do
		S (context, ST (fEnv, cEnv), fInfo, cInfo, lastLabel, regs, results, cStr) <- get
		case cInfo of
			CNone -> return args
			cInfo -> return $ (Arg (Class $ name cInfo) (Ident "this")):args

-- Context --

addjustTo16 x = ((x `div` 16) + (if x `mod` 16 /= 0 then 1 else 0)) * 16

isLoaded varInfo = location varInfo /= 0

enterContext (C ident None) context = C ident context
enterContext _ context = context

getFunIdent (F ident) = Just ident
getFunIdent (C ident context) = getFunIdent context
getFunIdent None = Nothing

funInfoWithEnv (FunInfo typ shift maxShift envs) newEnvs = (FunInfo typ shift maxShift newEnvs)
