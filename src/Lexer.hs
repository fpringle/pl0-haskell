module Lexer where

import Token


isLetter :: Char -> Bool
isLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

isNumber :: Char -> Bool
isNumber c = ('0' <= c && c <= '9')

isKeywordChar :: Char -> Bool
isKeywordChar c = isLetter c || isNumber c || c == '_'

lexIdent :: String -> String -> Maybe (Token, String)
lexIdent c "" = Just (Identifier c, "")
lexIdent c (first:rest)
  | isKeywordChar first   = lexIdent (c ++ [first]) rest
  | otherwise             = Just (Identifier c, first:rest)

lexNumber :: String -> String -> Maybe (Token, String)
lexNumber c "" = Just (Number (read c :: Int), "")
lexNumber c (first:rest)
  | isNumber first        = lexNumber (c ++ [first]) rest
  | isKeywordChar first   = Nothing
  | otherwise            = Just (Number (read c :: Int), first:rest)

lexFirst :: String -> Maybe (Token, String)
lexFirst "" = Nothing
-- keywords
lexFirst ('c':'o':'n':'s':'t':' ':rest) = Just (Const, rest)
lexFirst ('v':'a':'r':rest) = Just (Var, rest)
lexFirst ('p':'r':'o':'c':'e':'d':'u':'r':'e':rest) = Just (Procedure, rest)
lexFirst ('c':'a':'l':'l':rest) = Just (Call, rest)
lexFirst ('b':'e':'g':'i':'n':rest) = Just (Begin, rest)
lexFirst ('e':'n':'d':rest) = Just (End, rest)
lexFirst ('i':'f':rest) = Just (If, rest)
lexFirst ('t':'h':'e':'n':rest) = Just (Then, rest)
lexFirst ('w':'h':'i':'l':'e':rest) = Just (While, rest)
lexFirst ('d':'o':rest) = Just (Do, rest)
lexFirst ('o':'d':'d':rest) = Just (Odd, rest)
-- punctuation
lexFirst (',':rest) = Just(Comma, rest)
lexFirst (';':rest) = Just(Semicolon, rest)
lexFirst ('=':rest) = Just(Equals, rest)
lexFirst (':':'=':rest) = Just(ColonEquals, rest)
lexFirst ('.':rest) = Just(Dot, rest)
lexFirst ('?':rest) = Just(Question, rest)
lexFirst ('!':rest) = Just(Exclamation, rest)
lexFirst ('#':rest) = Just(Hash, rest)
lexFirst ('<':'=':rest) = Just(LessThanEquals, rest)
lexFirst ('<':rest) = Just(LessThan, rest)
lexFirst ('>':'=':rest) = Just(GreaterThanEquals, rest)
lexFirst ('>':rest) = Just(GreaterThan, rest)
lexFirst ('+':rest) = Just(Plus, rest)
lexFirst ('-':rest) = Just(Minus, rest)
lexFirst ('*':rest) = Just(Times, rest)
lexFirst ('/':rest) = Just(Divide, rest)
lexFirst ('(':rest) = Just(OpenParen, rest)
lexFirst (')':rest) = Just(CloseParen, rest)
-- values
lexFirst (first:rest)
  | first == '0'                    = Nothing
  | isLetter first || first == '_'  = lexIdent [first] rest
  | isNumber first                  = lexNumber [first] rest
  | otherwise                       = Nothing

lexPL0 :: String -> Maybe [Token]
lexPL0 "" = Just []
lexPL0 (' ':rest) = lexPL0 rest
lexPL0 other = do
  (first, rest) <- lexFirst other
  tl <- lexPL0 rest
  return (first : tl)
