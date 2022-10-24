{-# LANGUAGE DeriveFunctor #-}
module Syntax where


type Identifier = String
type Number = Int

-- syntax

data Expression a =
  UnaryPlus (Term a)
  | UnaryMinus (Term a)
  | BinaryPlus (Expression a) (Term a)
  | BinaryMinus (Expression a) (Term a)
  deriving (Eq, Functor)

instance Show a => Show (Expression a) where
  show (UnaryPlus t) = show t
  show (UnaryMinus t) = "- (" ++ show t ++ ")"
  show (BinaryPlus e t) = "(" ++ show e ++ ") + (" ++ show t ++ ")"
  show (BinaryMinus e t) = "(" ++ show e ++ ") - (" ++ show t ++ ")"

data Term a =
  SingleFactor (Factor a)
  | Mul (Term a) (Factor a)
  | Div (Term a) (Factor a)
  deriving (Eq, Functor)

instance Show a => Show (Term a) where
  show (SingleFactor f) = show f
  show (Mul t f) = show t ++ " * " ++ show f
  show (Div t f) = show t ++ " / " ++ show f

data Factor a =
  Ident a
  | Num Number
  | Parens (Expression a)
  deriving (Eq, Functor)

instance Show a => Show (Factor a) where
  show (Ident id) = show id
  show (Num n) = show n
  show (Parens e) = "(" ++ show e ++ ")"

data Op =
  OP_LT
  | OP_LTE
  | OP_GT
  | OP_GTE
  | OP_EQ
  | OP_HASH
  deriving (Eq)

instance Show Op where
  show OP_LT    = "<"
  show OP_LTE   = "<="
  show OP_GT    = ">"
  show OP_GTE   = ">="
  show OP_EQ    = "="
  show OP_HASH  = "#"

data Condition a =
  Odd (Expression a)
  | Comp (Expression a) Op (Expression a)
  deriving (Eq, Functor)

instance Show a => Show (Condition a) where
  show (Odd e) = "odd " ++ show e
  show (Comp e1 o e2) = show e1 ++ " " ++ show o ++ " " ++ show e2

data Statement a =
  Set a (Expression a)
  | Call a
  | Input a
  | Output (Expression a)
  | StmtBlock [Statement a]
  | IfStmt (Condition a) (Statement a)
  | WhileStmt (Condition a) (Statement a)
  | PopBlock      -- internal command used to return from procedure call
  deriving (Show, Eq, Functor)

data Block a = Block {
  constDecls    :: [(a, Number)]
  , varDecls    :: [a]
  , procDefs    :: [(a, Block a)]
  , body        :: Statement a
  } deriving (Show, Eq, Functor)

newtype Program a = Program (Block a)
  deriving (Show, Eq, Functor)

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
