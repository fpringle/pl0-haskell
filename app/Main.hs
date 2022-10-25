module Main where

import System.Environment
import Control.Monad
import Control.Monad.Trans.State
import System.IO
import System.Exit
import qualified Data.Map as Map

import Text.Parsec

import PL0.Lexer
import qualified PL0.Parser as P
import PL0.Syntax
import PL0.Interpreter hiding (runProgram)
import qualified PL0.Interpreter as I (runProgram)
import PL0.Compiler

usage :: IO ()
usage = die "USAGE: pl0 [run/compile] pl0_file"

runProgram :: Program Identifier -> IO ()
runProgram prog = do
  finalState <- I.runProgram prog
  when (hasError finalState) $ do
    putStrLn "Errors in execution:"
    mapM_ (\e -> putStrLn ("  " ++ e)) $ errors finalState
    putStrLn "Final machine state:"
    prettyPrintScope finalState
    mapM_ (\t -> mapM_ printIfNotNothing (Map.toList $ variables t) >> putStrLn "") $ tables finalState

  where printIfNotNothing :: (String, Maybe Int) -> IO ()
        printIfNotNothing (id, Just val) = putStrLn ("    " ++ id ++ " = " ++ show val)
        printIfNotNothing (id, Nothing) = putStrLn ("    " ++ id ++ " = undefined")

compileProgram :: Program Identifier -> IO ()
compileProgram prog = do
  let Program b = prog
  putStrLn "#######################################################"
  putStrLn "ORIGINAL PROGRAM"
  prettyPrintBlock b

  let scopedIDs = getAllSymbols prog

  let transformed = transformBlock scopedIDs b

  let smap = mapSymbolsToAddresses scopedIDs
  let (cmds, _, _, _, _) = eval smap transformed
  putStrLn "#######################################################"
  putStrLn "ASSEMBLY"
  mapM_ (\c -> let s = show c in putStrLn (if ':' `elem` s then '\n':s else s)) cmds

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) usage
  let [runOrCompile, progname] = args
  unless (runOrCompile `elem` ["run", "compile"]) usage
  program <- readFile progname
  let tokenlist = parse lexPL0 progname program
  case tokenlist of
    Left err   -> putStrLn ("Error in lexing: " ++ show err)
    Right list -> case parse P.parseProgram progname list of
      Left err -> die ("Error in parsing:" ++ show err)
      Right prog -> if runOrCompile == "run"
                    then runProgram prog
                    else compileProgram prog

