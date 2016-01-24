module Optimize where

import Compile
import Control.Monad.State
import LatteState
import Data.Map as M

data OptState = OS {varMap :: M.Map Object Object} deriving (Eq,Ord)

clearOptState :: OptState
clearOptState = OS (M.empty)

optimizeProg (AProgram defs) = do
	newDefs <- forM defs optimizeDef
	return (AProgram newDefs)

optimizeDef (AFun name instrs) = do
	instrs <- removeDeadCode instrs
	instrs <- removeUnnecessaryMoves instrs
	instrs <- removeUnnecessaryJumps instrs
	return (AFun name instrs)

optimizeDef def = do
	return def

removeUnnecessaryMoves (i:[]) = return $ i:[]
removeUnnecessaryMoves (i:j:ix) = do
	case (i,j) of
		(Instr AMov typ obj reg, Instr AMov typ' reg' reg1@(Reg _ _ _)) -> do
			if and [reg == reg', reg == raxReg] then do
				newInstr <- removeUnnecessaryMoves $ (Instr AMov typ obj reg1):ix
				return $ newInstr
			else do
				newInstr <- removeUnnecessaryMoves (j:ix)
				return $ i:newInstr
		(_,_) -> do
			newInstr <- removeUnnecessaryMoves (j:ix)
			return $ i:newInstr

removeUnnecessaryJumps (i:[]) = return (i:[])
removeUnnecessaryJumps ((Goto l1):(LabelDecl l2):ix) = do
	ret <- removeUnnecessaryJumps ((LabelDecl l2):ix)
	if l1 == l2 then
		return ret
	else
		return $ (Goto l1):ret
removeUnnecessaryJumps (i:j:ix) = do
	ret <- removeUnnecessaryJumps (j:ix)
	return (i:ret)

removeDeadCode [] = return []
removeDeadCode (i@(Goto label):ix) = do
	nx <- dropTillLabelDecl ix
	nx <- removeDeadCode nx
	return (i:nx)

removeDeadCode (i:ix) = do
	nx <- removeDeadCode ix
	return (i:nx)

dropTillLabelDecl [] = return []
dropTillLabelDecl (i@(LabelDecl l):ix) = return (i:ix)
dropTillLabelDecl (i:ix) = do
	nx <- dropTillLabelDecl ix
	return nx
