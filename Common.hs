module Common where

import AbsLatte

typeSize Int = 4
typeSize Bool = 1
typeSize Void = 0
typeSize Str = 8
typeSize _ = 4

opposite LTH = GTH
opposite LE = GE
opposite GTH = LTH
opposite GE = LE
opposite EQU = EQU
opposite NE = NE