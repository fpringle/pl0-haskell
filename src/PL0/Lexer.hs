{-|
Module      : PL0.Lexer
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

The lexing stage of the interpreter/compiler workflow transforms a
'String' into an array of 'Token's.
-}

module PL0.Lexer (
  -- * Lexing
  lexPL0
  ) where

import Control.Monad (liftM2)
import Data.Functor (($>))

import Text.Parsec
import Text.Parsec.String

import PL0.Token

lexIdent :: Parser Token
lexIdent = Identifier <$> liftM2 (:) letter (many (letter <|> digit <|> char '_'))

lexNumber :: Parser Token
lexNumber = Number . read <$> (number <* notFollowedBy (letter <|> char '_'))
  where number = liftM2 (:) (oneOf "123456789") (many digit) <|> string "0"

keyword :: String -> Parser String
keyword s = try (string s <* notFollowedBy (alphaNum <|> char '_'))

lexToken :: Parser Token
lexToken =
  (keyword "const" $> Const)
  <|> (keyword "var" $> Var)
  <|> (keyword "procedure" $> Procedure)
  <|> (keyword "call" $> Call)
  <|> (keyword "begin" $> Begin)
  <|> (keyword "end" $> End)
  <|> (keyword "if" $> If)
  <|> (keyword "then" $> Then)
  <|> (keyword "while" $> While)
  <|> (keyword "do" $> Do)
  <|> (keyword "odd" $> Odd)

  <|> try (string ":=" $> ColonEquals)
  <|> try (string "<=" $> LessThanEquals)
  <|> try (string ">=" $> GreaterThanEquals)

  <|> (char ',' $> Comma)
  <|> (char ';' $> Semicolon)
  <|> (char '=' $> Equals)
  <|> (char '.' $> Dot)
  <|> (char '?' $> Question)
  <|> (char '!' $> Exclamation)
  <|> (char '#' $> Hash)
  <|> (char '<' $> LessThan)
  <|> (char '>' $> GreaterThan)
  <|> (char '+' $> Plus)
  <|> (char '-' $> Minus)
  <|> (char '*' $> Times)
  <|> (char '/' $> Divide)
  <|> (char '(' $> OpenParen)
  <|> (char ')' $> CloseParen)

  <|> lexIdent
  <|> lexNumber

-- | A Parser representing a transformation of a String into a series of 'Token's.
lexPL0 :: Parser [Token]
lexPL0 = sepEndBy lexToken spaces
