module Predefined where

import AbsLatte

class AddPredefined a where
	addPredefined :: a -> a


instance AddPredefined Program where
	addPredefined (Program topDefs) = (Program (predefinedFunctions ++ topDefs))

predefinedFunctions = [preError, prePrintInt, prePrintString, preReadInt, preReadString]

preError = (TopFun (FnDef Void (Ident "error") [] (Block [])))
prePrintInt = (TopFun (FnDef Void (Ident "printInt") [Arg Int (Ident "i")] (Block [])))
prePrintString = (TopFun (FnDef Void (Ident "printString") [Arg Str (Ident "s")] (Block [])))
preReadInt = (TopFun (FnDef Int (Ident "readInt") [] (Block [])))
preReadString = (TopFun (FnDef Str (Ident "readString") [] (Block [])))

predefinedList = ["error", "printInt", "printString", "readInt", "readString"]
isPredefined ident = elem ident predefinedList

predefinedString = [("error_str","runtime error\n"), ("scan_schema", "%d"), ("print_schema", "%d\n"), ("scan_str_schema", "%s\n")]
