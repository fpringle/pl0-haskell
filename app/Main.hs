module Main where

import System.Environment

import Lexer
import Parser
import Syntax

prettyPrintBlock :: Block -> IO ()
prettyPrintBlock = go 0
  where
    go ind b = do
      let p s = putStrLn ((take ind $ repeat ' ') ++ s)
      p "constant declarations:"
      mapM_ (\(i,n) -> p ("  " ++ i ++ " := " ++ show n)) $ constDecls b
      p "variable declarations:"
      mapM_ (\i -> p ("  " ++ i)) $ varDecls b
      p "body:"
      p $ show $ body b
      p "procedures:"
      mapM_ (\(i, bl) -> p (i ++ ":") >> go (ind+4) bl) $ procDefs b


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
