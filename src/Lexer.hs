module Lexer where

import Data.Char (isDigit, isSpace)

import Token


isLetter :: Char -> Bool
isLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

isKeywordChar :: Char -> Bool
isKeywordChar c = isLetter c || isDigit c || c == '_'

lexIdent :: String -> String -> Either String (Token, String)
lexIdent c "" = Right (Identifier c, "")
lexIdent c (first:rest)
  | isKeywordChar first   = lexIdent (c ++ [first]) rest
  | otherwise             = Right (Identifier c, first:rest)

tryRead :: String -> Either String Int
tryRead "" = Left "empty number"
tryRead "0" = Right 0
tryRead ('0':_) = Left "number starting with zero"
tryRead s = Right $ read s

lexNumber :: String -> String -> Either String (Token, String)
lexNumber c "" = tryRead c >>= \n -> Right (Number n, "")
lexNumber c (first:rest)
  | isDigit first        = lexNumber (c ++ [first]) rest
  | isKeywordChar first   = Left ("keyword character " ++ show first ++ " in number")
  | otherwise            = tryRead c >>= \n -> Right (Number n, first:rest)

lexFirst :: String -> Either String (Token, String)
lexFirst "" = Left "Empty string"
-- keywords
lexFirst ('c':'o':'n':'s':'t':' ':rest) = Right (Const, rest)
lexFirst ('v':'a':'r':rest) = Right (Var, rest)
lexFirst ('p':'r':'o':'c':'e':'d':'u':'r':'e':rest) = Right (Procedure, rest)
lexFirst ('c':'a':'l':'l':rest) = Right (Call, rest)
lexFirst ('b':'e':'g':'i':'n':rest) = Right (Begin, rest)
lexFirst ('e':'n':'d':rest) = Right (End, rest)
lexFirst ('i':'f':rest) = Right (If, rest)
lexFirst ('t':'h':'e':'n':rest) = Right (Then, rest)
lexFirst ('w':'h':'i':'l':'e':rest) = Right (While, rest)
lexFirst ('d':'o':rest) = Right (Do, rest)
lexFirst ('o':'d':'d':rest) = Right (Odd, rest)
-- punctuation
lexFirst (',':rest) = Right (Comma, rest)
lexFirst (';':rest) = Right (Semicolon, rest)
lexFirst ('=':rest) = Right (Equals, rest)
lexFirst (':':'=':rest) = Right (ColonEquals, rest)
lexFirst ('.':rest) = Right (Dot, rest)
lexFirst ('?':rest) = Right (Question, rest)
lexFirst ('!':rest) = Right (Exclamation, rest)
lexFirst ('#':rest) = Right (Hash, rest)
lexFirst ('<':'=':rest) = Right (LessThanEquals, rest)
lexFirst ('<':rest) = Right (LessThan, rest)
lexFirst ('>':'=':rest) = Right (GreaterThanEquals, rest)
lexFirst ('>':rest) = Right (GreaterThan, rest)
lexFirst ('+':rest) = Right (Plus, rest)
lexFirst ('-':rest) = Right (Minus, rest)
lexFirst ('*':rest) = Right (Times, rest)
lexFirst ('/':rest) = Right (Divide, rest)
lexFirst ('(':rest) = Right (OpenParen, rest)
lexFirst (')':rest) = Right (CloseParen, rest)
-- values
lexFirst (first:rest)
  | isLetter first || first == '_'  = lexIdent [first] rest
  | isDigit first                   = lexNumber [first] rest
  | otherwise                       = Left ("Invalid token first char: " ++ show first)

lexPL0 :: String -> Either String [Token]
lexPL0 "" = Right []
lexPL0 (other:rest)
  | isSpace other = lexPL0 rest
  | otherwise = do
  (first, remainder) <- lexFirst (other:rest)
  tl <- lexPL0 remainder
  return (first : tl)
