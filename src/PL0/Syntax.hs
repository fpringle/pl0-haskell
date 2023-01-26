{-|
Module      : PL0.Syntax
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

PL/0 follows a typical expression-factor-term model.
For the full syntax specification in extended Backus-Naur form see [here](spec.ebnf).
-}

{-# LANGUAGE DeriveFunctor #-}

module PL0.Syntax (
  -- * Structure of a PL/0 program
  Program (..)
  , Block (..)
  , prettyPrintBlock
  -- * Statements in PL/0
  , Statement (..)
  , prettyPrintStatement

  -- * Expressions, terms and factors
  , Expression (..)
  , Term (..)
  , Factor (..)

  -- * Conditions
  , Condition (..)
  , Op (..)

  -- * Type aliases
  , Identifier
  , Number
  ) where


-- | PL/0 strings are represented by the 'String' datatype.
type Identifier = String

-- | PL/0 numbers are represented by the 'Int' datatype.
type Number = Int

-- | An expression in the PL/0 language.
data Expression a =
  -- | e.g. @+t@
  UnaryPlus (Term a)
  -- | e.g. @-t@
  | UnaryMinus (Term a)
  -- | e.g. @e+t@
  | BinaryPlus (Expression a) (Term a)
  -- | e.g. @e-t@
  | BinaryMinus (Expression a) (Term a)
  deriving (Eq, Functor)

instance Show a => Show (Expression a) where
  show (UnaryPlus t) = show t
  show (UnaryMinus t) = "- (" ++ show t ++ ")"
  show (BinaryPlus e t) = "(" ++ show e ++ ") + (" ++ show t ++ ")"
  show (BinaryMinus e t) = "(" ++ show e ++ ") - (" ++ show t ++ ")"

-- | A term in the PL/0 language.
data Term a =
  -- | e.g. @f@
  SingleFactor (Factor a)
  -- | e.g. @t*f@
  | Mul (Term a) (Factor a)
  -- | e.g. @t/f@
  | Div (Term a) (Factor a)
  deriving (Eq, Functor)

instance Show a => Show (Term a) where
  show (SingleFactor f) = show f
  show (Mul t f) = show t ++ " * " ++ show f
  show (Div t f) = show t ++ " / " ++ show f

-- | A factor in the PL/0 language.
data Factor a =
  -- | e.g. @x@
  Ident a
  -- | e.g. @2@
  | Num Number
  -- | e.g. @(e)@
  | Parens (Expression a)
  deriving (Eq, Functor)

instance Show a => Show (Factor a) where
  show (Ident id) = show id
  show (Num n) = show n
  show (Parens e) = "(" ++ show e ++ ")"

-- | Enum describing binary comparison operators on two numbers.
data Op =
  OP_LT
  | OP_LTE
  | OP_GT
  | OP_GTE
  | OP_EQ
  | OP_HASH
  deriving Eq

instance Show Op where
  show OP_LT    = "<"
  show OP_LTE   = "<="
  show OP_GT    = ">"
  show OP_GTE   = ">="
  show OP_EQ    = "="
  show OP_HASH  = "#"

-- | An expression that evaluates to a boolean.
data Condition a =
  -- | @Odd e@ evaluates to true if e evaluates to an odd number, else false.
  Odd (Expression a)
  -- | @Comp e1 op e2@ checks if e1, e2 satisfy the condition represented by an 'Op' type.
  | Comp (Expression a) Op (Expression a)
  deriving (Eq, Functor)

instance Show a => Show (Condition a) where
  show (Odd e) = "odd " ++ show e
  show (Comp e1 o e2) = show e1 ++ " " ++ show o ++ " " ++ show e2

-- | A statement in the PL/0 language.
data Statement a =
  -- | Set a variable to a value.
  Set a (Expression a)
  -- | Call a procedure.
  | Call a
  -- | Read a number from stdin and store in a variable.
  | Input a
  -- | Evaluate an expression and print the result to stdout.
  | Output (Expression a)
  -- | Execute a list of statements in series.
  | StmtBlock [Statement a]
  -- | If the condition evaluates to true, execute the statement.
  | IfStmt (Condition a) (Statement a)
  -- | While the condition evaluates to true, execute the statement.
  | WhileStmt (Condition a) (Statement a)
  -- | Internal command used to return from procedure call.
  | PopBlock
  deriving (Show, Eq, Functor)

-- | The main body of a PL/0 program is a Block. It contains:
--
--    * Zero or more constant declarations;
--
-- @
-- const x = 5;
-- @
--
--    * Zero or more non-initialized variable declarations;
--
-- @
-- var square;
-- @
--
--    * Zero or more procedure definitions;
--
-- @
-- procedure square;
-- begin
--     squ:= x * x
-- end;
-- @
--
--    * Exactly one 'Statement' which is like the \"main\" function.
--
-- @
-- begin
--     call square;
--     ! squ;
-- end.
-- @
data Block a = Block {
  -- | Constant declarations
  constDecls    :: [(a, Number)]
  -- | Variable declarations
  , varDecls    :: [a]
  -- | Procedure declarations
  , procDefs    :: [(a, Block a)]
  -- | Body statement
  , body        :: Statement a
  } deriving (Show, Eq, Functor)

-- | 'Program' is just a wrapper around 'Block'.
newtype Program a = Program (Block a)
  deriving (Show, Eq, Functor)

-- | Return a nice representation of a block, similar to the original PL/0 program.
prettyPrintBlock :: Show a => Block a -> IO ()
prettyPrintBlock = go 0
  where
    go :: Show a => Int -> Block a -> IO ()
    go ind b = do
      let p s = putStrLn (replicate ind ' ' ++ s)
      p "constant declarations:"
      mapM_ (\(i,n) -> p ("  " ++ show i ++ " = " ++ show n)) $ constDecls b
      p "variable declarations:"
      mapM_ (\i -> p ("  " ++ show i)) $ varDecls b
      p "body:"
      prettyPrintStatement (ind+2) $ body b
      p "procedures:"
      mapM_ (\(i, bl) -> p (show i ++ ":") >> go (ind+4) bl) $ procDefs b

-- | Return a nice representation of a statement, similar to the original PL/0 program.
prettyPrintStatement :: Show a => Int -> Statement a -> IO ()
prettyPrintStatement ind = format
  where
    p x = putStrLn (replicate ind ' ' ++ x)
    format :: Show a => Statement a -> IO ()
    format (Set id exp) = p (show id ++ " := " ++ show exp)
    format (Call id) = p ("call " ++ show id)
    format (Input id) = p ("read " ++ show id)
    format (Output exp) = p ("write " ++ show exp)
    format (StmtBlock s) = p "begin" >> mapM_ (prettyPrintStatement (ind+2)) s >> p "end"
    format (IfStmt cond s) = p ("if " ++ show cond) >> p "then" >> prettyPrintStatement (ind+2) s
    format (WhileStmt cond s) = p ("while " ++ show cond) >> p "do" >> prettyPrintStatement (ind+2) s
    format PopBlock = p "return"
