{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}
-- | After the lexing stage, we need to transform a list of 'T.Token's into an
-- abstract syntax tree (AST) representing a full PL/0 program.
module PL0.Parser (
  -- * Parser type
  PL0Parser

  -- ** Instances of 'PL0Parser'
  , parseExpression
  , parseTerm
  , parseFactor
  , parseOp
  , parseCondition
  , parseStatements
  , parseConsts
  , parseVars
  , parseProcs
  , parseBlock
  , parseProgram

  ) where

import Control.Monad (unless)
import qualified PL0.Syntax as S
import qualified PL0.Token as T

import Text.Parsec

-- | A parser that represents a transformation from a list of 'T.Token's.
--
-- If @p@ is a parser of type @'PL0Parser' ('S.Block' 'S.Identifier')@
-- and @tks@ is a token list of type @['T.Token']@, then
-- @'parse' p fp tks@ will give a value of type @Either 'ParseError' 'S.Block'@.
type PL0Parser a = Parsec [T.Token] () a

assertTopEq :: T.Token -> PL0Parser Bool
assertTopEq token = do
  top <- peek
  let res = top == token
  unless res $ unexpected ("expected " ++ show token ++ " but got " ++ show top)
  return res

assertTopType :: T.Token -> PL0Parser Bool
assertTopType token = do
  top <- peek
  let res = T.sameType top token
  unless res $ unexpected ("expected type " ++ show token ++ " but got " ++ show top)
  return res

popIdent :: PL0Parser S.Identifier
popIdent = do
  assertTopType (T.Identifier "")
  ident <- pop
  let T.Identifier id = ident
  return id

peek :: PL0Parser T.Token
peek = try $ lookAhead anyToken

pop :: PL0Parser T.Token
pop = anyToken

parseExprRecursive :: S.Expression S.Identifier -> PL0Parser (S.Expression S.Identifier)
parseExprRecursive lhs = do
  top <- peek
  case top of
    T.Plus   -> pop >> parseTerm >>= parseExprRecursive . S.BinaryPlus lhs
    T.Minus  -> pop >> parseTerm >>= parseExprRecursive . S.BinaryMinus lhs
    _         -> return lhs

-- | Parse an 'S.Expression'.
parseExpression :: PL0Parser (S.Expression S.Identifier)
parseExpression = do
  top <- peek
  cons <- case top of
    T.Minus -> pop >> return S.UnaryMinus
    T.Plus  -> pop >> return S.UnaryPlus
    _       -> return S.UnaryPlus
  term <- parseTerm
  let lhs = cons term
  parseExprRecursive lhs

parseTermRecursive :: S.Term S.Identifier -> PL0Parser (S.Term S.Identifier)
parseTermRecursive lhs = do
  top <- peek
  case top of
    T.Times   -> pop >> parseFactor >>= parseTermRecursive . S.Mul lhs
    T.Divide  -> pop >> parseFactor >>= parseTermRecursive . S.Div lhs
    _         -> return lhs

-- | Parse a 'S.Term'.
parseTerm :: PL0Parser (S.Term S.Identifier)
parseTerm = parseFactor >>= parseTermRecursive . S.SingleFactor

-- | Parse a 'S.Factor'.
parseFactor :: PL0Parser (S.Factor S.Identifier)
parseFactor = do
  top <- pop
  case top of
    T.Identifier s  -> return $ S.Ident s
    T.Number n      -> return $ S.Num n
    T.OpenParen     -> do
      expr <- parseExpression
      assertTopEq T.CloseParen
      pop
      return $ S.Parens expr
    _ -> do
      unexpected ("bad first token for factor: " ++ show top)
      return $ S.Ident ""

-- | Parse a binary comparison operator.
parseOp :: PL0Parser S.Op
parseOp = do
  top <- pop
  case top of
    T.Equals            -> return S.OP_EQ
    T.Hash              -> return S.OP_HASH
    T.LessThan          -> return S.OP_LT
    T.LessThanEquals    -> return S.OP_LTE
    T.GreaterThan       -> return S.OP_GT
    T.GreaterThanEquals -> return S.OP_GTE
    _ -> do
      unexpected ("expected comparison operator but got " ++ show top)
      return (error ("bad comp op: " ++ show top))

-- | Parse a 'S.Condition'.
parseCondition :: PL0Parser (S.Condition S.Identifier)
parseCondition = do
  top <- peek
  case top of
    T.Odd -> pop >> S.Odd <$> parseExpression
    _     -> do
      lhs <- parseExpression
      op <- parseOp
      S.Comp lhs op <$> parseExpression

-- | Parse a list of 'S.Statement's.
parseStatements :: PL0Parser [S.Statement S.Identifier]
parseStatements = do
  top <- peek
  case top of
    T.Semicolon -> do
      pop
      stmt <- parseStatement
      rest <- parseStatements
      return (stmt : rest)
    T.End -> pop >> return []
    _ -> do
      unexpected ("expected ';' or 'end' but got: " ++ show top)
      return []

parseStatement :: PL0Parser (S.Statement S.Identifier)
parseStatement = do
  top <- peek
  case top of
    T.Identifier id -> do
      pop
      assertTopEq T.ColonEquals
      pop
      S.Set id <$> parseExpression
    T.Call    -> do
      pop
      S.Call <$> popIdent
    T.Question -> do
      pop
      S.Input <$> popIdent
    T.Exclamation -> do
      pop
      S.Output <$> parseExpression
    T.Begin -> do
      pop
      first <- parseStatement
      rest <- parseStatements
      return $ S.StmtBlock (first : rest)
    T.If  -> do
      pop
      cond <- parseCondition
      assertTopEq T.Then
      pop
      S.IfStmt cond <$> parseStatement
    T.While -> do
      pop
      cond <- parseCondition
      assertTopEq T.Do
      pop
      S.WhileStmt cond <$> parseStatement
    _ -> do
      unexpected ("bad first token for statement: " ++ show top)
      return (S.Call "")

parseConst :: PL0Parser (S.Identifier, S.Number)
parseConst = do
  assertTopType (T.Identifier "")
  id <- popIdent
  pop
  assertTopType (T.Number 0)
  x <- pop
  let T.Number num = x
  return (id, num)

-- | Parse a list of constant declarations.
parseConsts :: PL0Parser [(S.Identifier, S.Number)]
parseConsts = do
  top <- peek
  case top of
    T.Const -> do
      pop
      first <- parseConst
      rest <- helper
      return (first : rest)
    _       -> return []

  where
    helper :: PL0Parser [(S.Identifier, S.Number)]
    helper = do
      next <- pop
      case next of
        T.Comma -> do
          one <- parseConst
          others <- helper
          return (one : others)
        T.Semicolon -> return []
        _ -> do
          unexpected ("bad first token for const declaration: " ++ show next)
          return []

parseVar :: PL0Parser S.Identifier
parseVar = popIdent

-- | Parse a list of variable declarations.
parseVars :: PL0Parser [S.Identifier]
parseVars = do
  top <- peek
  case top of
    T.Var -> do
      pop
      first <- parseVar
      rest <- helper
      return (first : rest)
    _       -> return []

  where
    helper :: PL0Parser [S.Identifier]
    helper = do
      next <- pop
      case next of
        T.Comma -> do
          one <- parseVar
          others <- helper
          return (one : others)
        T.Semicolon -> return []
        _ -> do
          unexpected ("bad first token for const declaration: " ++ show next)
          return []

parseProc :: PL0Parser (S.Identifier, S.Block S.Identifier)
parseProc = do
  assertTopType (T.Identifier "")
  ident <- popIdent
  assertTopEq T.Semicolon
  pop
  block <- parseBlock
  assertTopEq T.Semicolon
  pop
  return (ident, block)

-- | Parse a list of procedure definitions.
parseProcs :: PL0Parser [(S.Identifier, S.Block S.Identifier)]
parseProcs = do
  top <- peek
  case top of
    T.Procedure -> do
      pop
      proc <- parseProc
      rest <- parseProcs
      return (proc : rest)
    _ -> return []

-- | Parse a 'S.Block'.
parseBlock :: PL0Parser (S.Block S.Identifier)
parseBlock = S.Block <$> parseConsts <*> parseVars <*> parseProcs <*> parseStatement

-- | Parse a 'S.Program'.
parseProgram :: PL0Parser (S.Program S.Identifier)
parseProgram = do
  bl <- parseBlock
  assertTopEq T.Dot
  pop
  return $ S.Program bl
