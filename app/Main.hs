module Main where

import System.Environment

import Lexer
import Parser
import Syntax



main :: IO ()
main = do
  args <- getArgs
  let progname = head args
  putStrLn progname
  program <- readFile progname
  let tokenlist = lexPL0 program
  case tokenlist of
    Left err   -> putStrLn ("Error in lexing: " ++ err)
    Right list -> do
      -- mapM_ print list
      case parse list of
        Left errors -> do
          putStrLn "Errors in parsing:"
          mapM_ putStrLn errors
        Right prog -> do
          putStrLn "Parsed sucessfully:"
          print prog
          let Program b = prog
          prettyPrintBlock b
