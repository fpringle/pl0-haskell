{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}
{-# LANGUAGE TupleSections #-}
-- | The PL/0 interpreter is a state machine that executes steps one by one
-- and updates its own internal memory accordingly.
module PL0.Interpreter (
  -- * Scopes
  Scope (..)
  , prettyPrintScope


  -- * Symbols
  , IdMap
  , SymbolTable (..)

  
  -- * The interpreter
  , Interpreter
  , hasError
  -- ** Running the interpreter
  , runProgram
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import Data.Functor ((<&>))
import qualified Data.Map as Map

import PL0.Syntax

-- | A polymorphic type for objects visible in the current scope.
type IdMap a = Map.Map Identifier a

-- | A representation of all the objects visible in the current scope
data SymbolTable = SymbolTable {
  -- | the constants currently visible
  constants         :: IdMap Int
  -- | the variables (initialized or non-initialized) currently visible
  , variables       :: IdMap (Maybe Int)
  -- | the procedures currently visible
  , functions       :: IdMap (Block Identifier)
  } deriving (Show)

lookupSymbolInTable :: Identifier -> SymbolTable -> Either String Int
lookupSymbolInTable id table =
  case Map.lookup id $ constants table of
    Just num  -> Right num
    Nothing -> case Map.lookup id $ variables table of
      Just (Just num2)  -> Right num2
      Just Nothing      -> Left ("tried to access uninitialised variable: " ++ id)
      Nothing           -> Left ("unknown symbol: " ++ id)

lookupSymbolInTables :: Identifier -> [SymbolTable] -> Either String Int
lookupSymbolInTables id [] = Left ("unknown symbol: " ++ id)
lookupSymbolInTables id (t:ts) = 
  case lookupSymbolInTable id t of
    Right num -> Right num
    _         -> lookupSymbolInTables id ts

lookupFunctionInTable :: Identifier -> SymbolTable -> Either String (Block Identifier)
lookupFunctionInTable id table =
  case Map.lookup id $ functions table of
    Just block   -> Right block
    Nothing      -> Left ("unknown procedure: " ++ id)

lookupFunctionInTables :: Identifier -> [SymbolTable] -> Either String (Block Identifier)
lookupFunctionInTables id [] = Left ("unknown function: " ++ id)
lookupFunctionInTables id (t:ts) = 
  case lookupFunctionInTable id t of
    Right bl  -> Right bl
    _         -> lookupFunctionInTables id ts

modifySymbolInTable :: Identifier -> Int -> SymbolTable -> Either String SymbolTable
modifySymbolInTable id val table =
  case Map.lookup id $ constants table of
    Just num  -> Left ("symbol " ++ id ++ " declared as constant")
    Nothing -> case Map.lookup id $ variables table of
      Just _  -> let newVars = Map.insert id (Just val) $ variables table
                 in Right $ table { variables = newVars }
      Nothing           -> Left ("unknown symbol: " ++ id)

modifySymbolInTables :: Identifier -> Int -> [SymbolTable] -> Either String [SymbolTable]
modifySymbolInTables id val [] = Left ("unknown symbol: " ++ id)
modifySymbolInTables id val (t:ts) = 
  case modifySymbolInTable id val t of
    Right newt -> Right (newt : ts)
    _         -> do
      rest <- modifySymbolInTables id val ts
      return (t : rest)

-- | The 'Scope' type represents the current state of the program.
data Scope = Scope {
  -- | symbols currently visible
  tables        :: [SymbolTable]
  -- | list of statements to execute
  , statements  :: [Statement Identifier]
  -- | any errors that have been reported
  , errors      :: [String]
  }

-- | Check if any errors have been reported yet.
hasError :: Scope -> Bool
hasError = not . null . errors

-- | Format a scope nicely and print it to stdout.
prettyPrintScope :: Scope -> IO ()
prettyPrintScope s = do
  let ts = tables s
  putStrLn "Tables:"
  print ts
  putStrLn "  consts:"
  mapM_ (\t -> mapM_ (\(id,val) -> putStrLn ("    " ++ id ++ " := " ++ show val)) (Map.toList $ constants t) >> putStrLn "") ts
  putStrLn "  vars:"
  mapM_ (\t -> mapM_ (\(id,val) -> putStrLn ("    " ++ id ++ " = " ++ show val)) (Map.toList $ variables t) >> putStrLn "") ts
  putStrLn "  procs:"
  mapM_ (\t -> mapM_ (\(f,_) -> putStrLn ("    " ++ f)) (Map.toList $ functions t) >> putStrLn "") ts

-- | A state machine representing the process of executing a PL/0 program.
type Interpreter a = StateT Scope IO a

-- | Smart constructor for an 'Interpreter'.
interpreter :: Program Identifier -> Scope
interpreter (Program b) = pushBlock (Scope [] [] []) b

run :: Interpreter ()
run = do
  cur <- get
  unless (hasError cur) $ case statements cur of
    []  -> return ()
    (stmt:rest) -> do
      put $ cur { statements = rest }
      -- lift $ putStrLn ("executing statement: " ++ show stmt)
      executeStmt stmt
      run

-- | Run all the steps of a program. 'runProgram' returns an IO action since we may
-- need to get input from the user and print output to the screen. The IO action returns
-- the final state of the interpreter.
runProgram :: Program Identifier -> IO Scope
runProgram = execStateT run . interpreter

pushBlock :: Scope -> Block Identifier -> Scope
pushBlock s b =
  let
    ts = tables s
    ss = statements s
    consts = Map.fromList $ constDecls b
    vars   = Map.fromList $ map (, Nothing) $ varDecls b
    funcs  = Map.fromList $ procDefs b
    newts = SymbolTable consts vars funcs : ts
  in
    s { tables = newts, statements = body b:PopBlock:ss }

evaluateFactor :: [SymbolTable] -> Factor Identifier -> Either String Int
evaluateFactor ts (Ident id) = lookupSymbolInTables id ts
evaluateFactor ts (Num n)    = Right n
evaluateFactor ts (Parens e) = evaluateExpression ts e

evaluateExpression :: [SymbolTable] -> Expression Identifier -> Either String Int
evaluateExpression ts (UnaryPlus t) = evaluateTerm ts t
evaluateExpression ts (UnaryMinus t) = do
  tosub <- evaluateTerm ts t
  return (-tosub)
evaluateExpression ts (BinaryPlus e t) = do
  lhs <- evaluateExpression ts e
  rhs <- evaluateTerm ts t
  return (lhs + rhs)
evaluateExpression ts (BinaryMinus e t) = do
  lhs <- evaluateExpression ts e
  rhs <- evaluateTerm ts t
  return (lhs - rhs)

evaluateTerm :: [SymbolTable] -> Term Identifier -> Either String Int
evaluateTerm ts (SingleFactor f) = evaluateFactor ts f
evaluateTerm ts (Mul t f) = do
  lhs <- evaluateTerm ts t
  rhs <- evaluateFactor ts f
  return (lhs * rhs)
evaluateTerm ts (Div t f) = do
  lhs <- evaluateTerm ts t
  rhs <- evaluateFactor ts f
  return (lhs `div` rhs)

evaluateCondition :: [SymbolTable] -> Condition Identifier -> Either String Bool
evaluateCondition ts (Odd e) = evaluateExpression ts e <&> odd
evaluateCondition ts (Comp e1 o e2) = do
  lhs <- evaluateExpression ts e1
  rhs <- evaluateExpression ts e2
  case o of
    OP_LT   -> return (lhs < rhs)
    OP_LTE  -> return (lhs <= rhs)
    OP_GT   -> return (lhs > rhs)
    OP_GTE  -> return (lhs >= rhs)
    OP_EQ   -> return (lhs == rhs)
    OP_HASH -> return (lhs /= rhs)

appendError :: String -> Interpreter ()
appendError s = modify (\cur -> cur { errors = errors cur ++ [s] })

executeStmt :: Statement Identifier -> Interpreter ()
executeStmt (Set id exp) = do
  cur <- get
  let ts = tables cur
  case evaluateExpression ts exp of
    Left error -> appendError error
    Right val -> case modifySymbolInTables id val ts of
      Left error2 -> appendError error2
      Right newTables -> put $ cur { tables = newTables }

executeStmt (Call id) = do
  cur <- get
  let ts = tables cur
  case lookupFunctionInTables id ts of
    Left error  -> appendError error
    Right block -> modify (`pushBlock` block)

executeStmt (Input id) = do
  lift $ putStrLn ("Enter " ++ id ++ ":")
  line <- lift getLine
  case readMaybe line :: Maybe Int of
    Nothing   -> appendError ("Invalid number input: " ++ line)
    Just val  -> do
      cur <- get
      let ts = tables cur
      case modifySymbolInTables id val ts of
        Left err    -> appendError err
        Right newts -> put $ cur { tables = newts }

executeStmt (Output exp) = do
  cur <- get
  let ts = tables cur
  case evaluateExpression ts exp of
    Left error -> lift (putStrLn ("error: " ++ show error)) >> appendError error
    Right val -> lift $ print val

executeStmt (StmtBlock stmts) = modify (\cur -> cur { statements = stmts ++ statements cur})

executeStmt (IfStmt cond stmt) = do
  cur <- get
  let ts = tables cur
  case evaluateCondition ts cond of
    Left err -> appendError err
    Right bool -> when bool $ modify (\cur2 -> cur2 { statements = stmt : statements cur2})

executeStmt (WhileStmt cond stmt) = do
  cur <- get
  let ts = tables cur
  case evaluateCondition ts cond of
    Left err -> appendError err
    Right bool -> when bool $ modify (\cur2 -> cur2 { statements = stmt : WhileStmt cond stmt : statements cur2})

executeStmt PopBlock = modify (\scope -> scope { tables = tail $ tables scope })
