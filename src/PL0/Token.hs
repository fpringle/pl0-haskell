{-|
Module      : PL0.Token
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

PL0.Token exports a datatype 'Token' which represents tokens used in the tokenizing stage.
-}

module PL0.Token (
  -- * Tokens used in parsing
  Token (..)
  -- * Utility functions
  , sameType
  ) where


-- | Token is an intermediate datatype used in the tokenizing/lexing stage.
-- There are different tokens representing keywords, punctuation, mathematical
-- operators, and atomic values.
data Token =
  -- keywords
  Const
  | Var
  | Procedure
  | Call
  | Begin
  | End
  | If
  | Then
  | While
  | Do
  | Odd

  -- punctuation
  | Comma
  | Semicolon
  | Equals
  | ColonEquals
  | Dot
  | Question
  | Exclamation
  | Hash
  | LessThan
  | LessThanEquals
  | GreaterThan
  | GreaterThanEquals
  | Plus
  | Minus
  | Times
  | Divide
  | OpenParen
  | CloseParen

  -- values
  | Identifier String
  | Number Int

  deriving (Show, Eq)

-- | Check if two tokens are of the same type.
sameType :: Token -> Token -> Bool
sameType (Identifier _) (Identifier _) = True
sameType (Number _) (Number _) = True
sameType x y = x == y
