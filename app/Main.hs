module Main where

import System.Environment
import Control.Monad
import Control.Monad.Trans.State

import Text.Parsec

import Lexer
import qualified Parser as P
import Syntax
import Interpreter
import Compiler


main :: IO ()
main = do
  args <- getArgs
  let progname = head args
  program <- readFile progname
  let tokenlist = parse lexPL0 progname program
  case tokenlist of
    Left err   -> putStrLn ("Error in lexing: " ++ show err)
    Right list -> case parse P.parseProgram progname list of
        Left err -> do
          putStrLn "Error in parsing:"
          print err
        Right prog -> do
          let Program b = prog
          let machine = interpreter prog
          finalState <- execStateT run machine
 {-
          when (hasError finalState) $ do
            putStrLn "Errors in execution:"
            mapM_ (\e -> putStrLn ("  " ++ e)) $ errors finalState
            putStrLn "Final machine state:"
            prettyPrintScope finalState
-}

          let Program b = prog
          putStrLn "#######################################################"
          putStrLn "original program:"
          prettyPrintBlock b

          let scopedIDs = getAllSymbols prog
          -- putStrLn "#######################################################"
          -- putStrLn "scoped IDs:"
          -- mapM_ print scopedIDs

          let transformed = transformBlock scopedIDs b
          -- putStrLn "#######################################################"
          -- putStrLn "transformed program:"
          -- prettyPrintBlock transformed
  
          let smap = mapSymbolsToAddresses scopedIDs
          let (cmds, _, _, _, _) = eval smap transformed
          putStrLn "#######################################################"
          putStrLn "assembly:"
          mapM_ (\c -> let s = show c in putStrLn (if ':' `elem` s then '\n':s else s)) cmds
          when (length args > 1) $ do
            let oname = head $ tail args
            mapM_ (\c -> appendFile oname (show c ++ "\n")) cmds
