module Parser where

import Control.Monad (unless)
import qualified Token as T
import qualified Syntax as S

import Text.Parsec

type Pl0Parser = Parsec [T.Token] ()

assertTopEq :: T.Token -> Pl0Parser Bool
assertTopEq token = do
  top <- peek
  let res = top == token
  unless res $ unexpected ("expected " ++ show token ++ " but got " ++ show top)
  return res

assertTopType :: T.Token -> Pl0Parser Bool
assertTopType token = do
  top <- peek
  let res = T.sameType top token
  unless res $ unexpected ("expected type " ++ show token ++ " but got " ++ show top)
  return res

popIdent :: Pl0Parser S.Identifier
popIdent = do
  assertTopType (T.Identifier "")
  ident <- pop
  let T.Identifier id = ident
  return id

peek :: Pl0Parser T.Token
peek = try $ lookAhead anyToken

pop :: Pl0Parser T.Token
pop = anyToken

parseExprRecursive :: S.Expression S.Identifier -> Pl0Parser (S.Expression S.Identifier)
parseExprRecursive lhs = do
  top <- peek
  case top of
    T.Plus   -> pop >> parseTerm >>= parseExprRecursive . S.BinaryPlus lhs
    T.Minus  -> pop >> parseTerm >>= parseExprRecursive . S.BinaryMinus lhs
    _         -> return lhs

parseExpression :: Pl0Parser (S.Expression S.Identifier)
parseExpression = do
  top <- peek
  cons <- case top of
    T.Minus -> pop >> return S.UnaryMinus
    T.Plus  -> pop >> return S.UnaryPlus
    _       -> return S.UnaryPlus
  term <- parseTerm
  let lhs = cons term
  parseExprRecursive lhs

parseTermRecursive :: S.Term S.Identifier -> Pl0Parser (S.Term S.Identifier)
parseTermRecursive lhs = do
  top <- peek
  case top of
    T.Times   -> pop >> parseFactor >>= parseTermRecursive . S.Mul lhs
    T.Divide  -> pop >> parseFactor >>= parseTermRecursive . S.Div lhs
    _         -> return lhs

parseTerm :: Pl0Parser (S.Term S.Identifier)
parseTerm = do
  fac <- parseFactor
  let lhs = S.SingleFactor fac
  parseTermRecursive lhs

parseFactor :: Pl0Parser (S.Factor S.Identifier)
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

parseOp :: Pl0Parser S.Op
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

parseCondition :: Pl0Parser (S.Condition S.Identifier)
parseCondition = do
  top <- peek
  case top of
    T.Odd -> pop >> S.Odd <$> parseExpression
    _     -> do
      lhs <- parseExpression
      op <- parseOp
      S.Comp lhs op <$> parseExpression

parseStatements :: Pl0Parser [S.Statement S.Identifier]
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

parseStatement :: Pl0Parser (S.Statement S.Identifier)
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

parseConst :: Pl0Parser (S.Identifier, S.Number)
parseConst = do
  assertTopType (T.Identifier "")
  id <- popIdent
  pop
  assertTopType (T.Number 0)
  x <- pop
  let T.Number num = x
  return (id, num)

parseConsts :: Pl0Parser [(S.Identifier, S.Number)]
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
    helper :: Pl0Parser [(S.Identifier, S.Number)]
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

parseVar :: Pl0Parser S.Identifier
parseVar = popIdent

parseVars :: Pl0Parser [S.Identifier]
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
    helper :: Pl0Parser [S.Identifier]
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

parseProc :: Pl0Parser (S.Identifier, S.Block S.Identifier)
parseProc = do
  assertTopType (T.Identifier "")
  ident <- popIdent
  assertTopEq T.Semicolon
  pop
  block <- parseBlock
  assertTopEq T.Semicolon
  pop
  return (ident, block)

parseProcs :: Pl0Parser [(S.Identifier, S.Block S.Identifier)]
parseProcs = do
  top <- peek
  case top of
    T.Procedure -> do
      pop
      proc <- parseProc
      rest <- parseProcs
      return (proc : rest)
    _ -> return []

parseBlock :: Pl0Parser (S.Block S.Identifier)
parseBlock = do
  consts <- parseConsts
  vars <- parseVars
  procs <- parseProcs
  S.Block consts vars procs <$> parseStatement

parseProgram :: Pl0Parser (S.Program S.Identifier)
parseProgram = do
  bl <- parseBlock
  assertTopEq T.Dot
  pop
  return $ S.Program bl
