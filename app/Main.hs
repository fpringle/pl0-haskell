module Main where

import System.Environment
import Control.Monad
import Control.Monad.Trans.State

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
  let tokenlist = lexPL0 program
  case tokenlist of
    Left err   -> putStrLn ("Error in lexing: " ++ err)
    Right list -> case P.parse list of
        Left errors -> do
          putStrLn "Errors in parsing:"
          mapM_ putStrLn errors
        Right prog -> do
{-
          let Program b = prog
          let machine = interpreter prog
          finalState <- execStateT run machine
          if hasError finalState
          then do
            putStrLn "Errors in execution:"
            mapM_ (\e -> putStrLn ("  " ++ e)) $ errors finalState
            putStrLn "Final machine state:"
            prettyPrintScope finalState
          else
            return ()
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
          mapM_ (\c -> let s = show c in putStrLn (if elem ':' s then '\n':s else s)) cmds
          when (length args > 1) $ do
            let oname = head $ tail args
            mapM_ (\c -> appendFile oname (show c ++ "\n")) cmds
