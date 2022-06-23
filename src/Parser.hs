module Parser where

import Control.Monad.State
import Data.Functor ((<&>))
import qualified Token as T
import qualified Syntax as S


data Parser = Parser {
  stack     :: [T.Token]
  , good    :: Bool
  , errors  :: [String]
  }

type ParserT a = State Parser a

parse :: [T.Token] -> Either [String] (S.Program S.Identifier)
parse tokens =
  let
    parser = Parser {
      stack     = tokens
      , good    = True
      , errors  = []
      }
    (prog, finalState) = runState parseProgram parser
  in
    if good finalState
    then Right prog
    else Left $ errors finalState

peek :: ParserT T.Token
peek = get <&> (head . stack)

empty :: ParserT Bool
empty = get <&> (null . stack)

pop :: ParserT T.Token
pop = do
  cur <- get
  let (hd:tl) = stack cur
  put $ cur { stack = tl }
  return hd

failure :: String -> ParserT ()
failure err = do
  cur <- get
  let nexts = take 10 $ stack cur
  let errmsg = err ++ "(remaining stack: " ++ show nexts ++ ")"
  put $ cur { good = False, errors = errors cur ++ [errmsg] }

assertTopEq :: T.Token -> ParserT Bool
assertTopEq token = do
  top <- peek
  let res = top == token
  unless res $ failure ("expected " ++ show token ++ " but got " ++ show top)
  return res

assertTopType :: T.Token -> ParserT Bool
assertTopType token = do
  top <- peek
  let res = T.sameType top token
  unless res $ failure ("expected type " ++ show token ++ " but got " ++ show top)
  return res

popIdent :: ParserT S.Identifier
popIdent = do
  assertTopType (T.Identifier "")
  ident <- pop
  let T.Identifier id = ident
  return id

parseExprRecursive :: S.Expression S.Identifier -> ParserT (S.Expression S.Identifier)
parseExprRecursive lhs = do
  top <- peek
  case top of
    T.Plus   -> pop >> parseTerm >>= parseExprRecursive . S.BinaryPlus lhs
    T.Minus  -> pop >> parseTerm >>= parseExprRecursive . S.BinaryMinus lhs
    _         -> return lhs

parseExpression :: ParserT (S.Expression S.Identifier)
parseExpression = do
  top <- peek
  cons <- case top of
    T.Minus -> pop >> return S.UnaryMinus
    T.Plus  -> pop >> return S.UnaryPlus
    _       -> return S.UnaryPlus
  term <- parseTerm
  let lhs = cons term
  parseExprRecursive lhs

parseTermRecursive :: S.Term S.Identifier -> ParserT (S.Term S.Identifier)
parseTermRecursive lhs = do
  top <- peek
  case top of
    T.Times   -> pop >> parseFactor >>= parseTermRecursive . S.Mul lhs
    T.Divide  -> pop >> parseFactor >>= parseTermRecursive . S.Div lhs
    _         -> return lhs

parseTerm :: ParserT (S.Term S.Identifier)
parseTerm = do
  fac <- parseFactor
  let lhs = S.SingleFactor fac
  parseTermRecursive lhs

parseFactor :: ParserT (S.Factor S.Identifier)
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
      failure ("bad first token for factor: " ++ show top)
      return $ S.Ident ""

parseOp :: ParserT S.Op
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
      failure ("expected comparison operator but got " ++ show top)
      return (error ("bad comp op: " ++ show top))

parseCondition :: ParserT (S.Condition S.Identifier)
parseCondition = do
  top <- peek
  case top of
    T.Odd -> pop >> parseExpression <&> S.Odd
    _     -> do
      lhs <- parseExpression
      op <- parseOp
      S.Comp lhs op <$> parseExpression

parseStatements :: ParserT [S.Statement S.Identifier]
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
      failure ("expected ';' or 'end' but got: " ++ show top)
      return []

parseStatement :: ParserT (S.Statement S.Identifier)
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
      failure ("bad first token for statement: " ++ show top)
      return (S.Call "")

parseConst :: ParserT (S.Identifier, S.Number)
parseConst = do
  assertTopType (T.Identifier "")
  id <- popIdent
  pop
  assertTopType (T.Number 0)
  x <- pop
  let T.Number num = x
  return (id, num)

parseConsts :: ParserT [(S.Identifier, S.Number)]
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
    helper :: ParserT [(S.Identifier, S.Number)]
    helper = do
      next <- pop
      case next of 
        T.Comma -> do
          one <- parseConst
          others <- helper
          return (one : others)
        T.Semicolon -> return []
        _ -> do
          failure ("bad first token for const declaration: " ++ show next)
          return []

parseVar :: ParserT S.Identifier
parseVar = popIdent

parseVars :: ParserT [S.Identifier]
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
    helper :: ParserT [S.Identifier]
    helper = do
      next <- pop
      case next of 
        T.Comma -> do
          one <- parseVar
          others <- helper
          return (one : others)
        T.Semicolon -> return []
        _ -> do
          failure ("bad first token for const declaration: " ++ show next)
          return []

parseProc :: ParserT (S.Identifier, S.Block S.Identifier)
parseProc = do
  assertTopType (T.Identifier "")
  ident <- popIdent
  assertTopEq T.Semicolon
  pop
  block <- parseBlock
  assertTopEq T.Semicolon
  pop
  return (ident, block)

parseProcs :: ParserT [(S.Identifier, S.Block S.Identifier)]
parseProcs = do
  top <- peek
  case top of
    T.Procedure -> do
      pop
      proc <- parseProc
      rest <- parseProcs
      return (proc : rest)
    _ -> return []

parseBlock :: ParserT (S.Block S.Identifier)
parseBlock = do
  consts <- parseConsts
  vars <- parseVars
  procs <- parseProcs
  S.Block consts vars procs <$> parseStatement

parseProgram :: ParserT (S.Program S.Identifier)
parseProgram = do
  bl <- parseBlock
  assertTopEq T.Dot
  pop
  return $ S.Program bl
