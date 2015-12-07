module TypeCollector where

import Monad.Control.State
import Data.Map as M

import LatteState

class TypeCollector a where
	collectTypes a = MonadState State m => a -> m b


