module CompileLatte where

import Data.Map as M
import Data.Set as S

type Reg = Int

type VEnv = M.Map Ident Reg
type FEnv = M.Map Ident [Arg]
type CEnv = M.Map Ident [Arg] [FEnv]

data Store = ST (VEnv, FEnv, CEnv) deriving (Eq, Ord, Show)


class Compile a where
	compile a -> StateT Store IO b

