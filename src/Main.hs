-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents, hPutStrLn, stderr )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath.Posix
import System.Exit
import System.Process
import Control.Monad.State

import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import TypeChecker
import TypeCollector
import Compile
import ConstEval
import Predefined
import LatteState
import ErrM
import Optimize

compileProg :: Program -> Err String
compileProg prog' = do
  let prog = addPredefined prog' in do
    state <- execStateT (collectTypes prog) $ clearState
    state <- execStateT (checkType prog) $ state
    (prog, state) <- runStateT (evalConst prog) $ state
    (aprog, state) <- runStateT (genProg' prog) state
    --(aprog, state') <- runStateT (optimizeProg aprog) $ clearOptState
    return $ show aprog


type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p f

run :: Verbosity -> ParseFun Program -> FilePath -> String -> IO ()
run v p f s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do 
                          case compileProg tree of
                            Bad s -> do 
                              hPutStrLn stderr $ "ERROR"
                              putStrLn s
                              exitFailure
                            Ok prog -> do
                              hPutStrLn stderr $ "OK"
                              writeFile (replaceExtension f "s") prog
                              (exitCode, sout, serr) <- readProcessWithExitCode "gcc" ["-o",(dropExtension f),(replaceExtension f "s")] ""
                              case exitCode of
                                ExitSuccess -> exitSuccess
                                _ -> do
                                  putStrLn sout
                                  putStrLn serr
                                  exitFailure



showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs





