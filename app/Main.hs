module Main where

import Lexer

main :: IO ()
main = do
  program <- getLine
  let tokenlist = lexPL0 program
  case tokenlist of
    Nothing   -> putStrLn "bad parse :("
    Just list -> mapM_ print list
