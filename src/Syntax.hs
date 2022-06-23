module Syntax where


type Identifier = String
type Number = Int

-- syntax

data Expression =
  UnaryPlus Term
  | UnaryMinus Term
  | BinaryPlus Expression Term
  | BinaryMinus Expression Term
  deriving (Eq)

instance Show Expression where
  show (UnaryPlus t) = show t
  show (UnaryMinus t) = "- (" ++ show t ++ ")"
  show (BinaryPlus e t) = "(" ++ show e ++ ") + (" ++ show t ++ ")"
  show (BinaryMinus e t) = "(" ++ show e ++ ") - (" ++ show t ++ ")"

data Term =
  SingleFactor Factor
  | Mul Term Factor
  | Div Term Factor
  deriving (Eq)

instance Show Term where
  show (SingleFactor f) = show f
  show (Mul t f) = show t ++ " * " ++ show f
  show (Div t f) = show t ++ " / " ++ show f

data Factor =
  Ident Identifier
  | Num Number
  | Parens Expression
  deriving (Eq)

instance Show Factor where
  show (Ident id) = id
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

data Condition =
  Odd Expression
  | Comp Expression Op Expression
  deriving (Eq)

instance Show Condition where
  show (Odd e) = "odd " ++ show e
  show (Comp e1 o e2) = show e1 ++ " " ++ show o ++ " " ++ show e2

data Statement =
  Set Identifier Expression
  | Call Identifier
  | Input Identifier
  | Output Expression
  | StmtBlock [Statement]
  | IfStmt Condition Statement
  | WhileStmt Condition Statement
  | PopBlock      -- internall command used to return from procedure call
  deriving (Show, Eq)

data Block = Block {
  constDecls    :: [(Identifier, Number)]
  , varDecls    :: [Identifier]
  , procDefs    :: [(Identifier, Block)]
  , body        :: Statement
  } deriving (Show, Eq)

newtype Program = Program Block
  deriving (Show, Eq)



prettyPrintBlock :: Block -> IO ()
prettyPrintBlock = go 0
  where
    go ind b = do
      let p s = putStrLn (replicate ind ' ' ++ s)
      p "constant declarations:"
      mapM_ (\(i,n) -> p ("  " ++ i ++ " = " ++ show n)) $ constDecls b
      p "variable declarations:"
      mapM_ (\i -> p ("  " ++ i)) $ varDecls b
      p "body:"
      prettyPrintStatement (ind+2) $ body b
      p "procedures:"
      mapM_ (\(i, bl) -> p (i ++ ":") >> go (ind+4) bl) $ procDefs b

prettyPrintStatement :: Int -> Statement -> IO ()
prettyPrintStatement ind = format
  where
    p x = putStrLn (replicate ind ' ' ++ x)
    format :: Statement -> IO ()
    format (Set id exp) = p (id ++ " := " ++ show exp)
    format (Call id) = p ("call " ++ id)
    format (Input id) = p ("read " ++ id)
    format (Output exp) = p ("write " ++ show exp)
    format (StmtBlock s) = p "begin" >> mapM_ (prettyPrintStatement (ind+2)) s >> p "end"
    format (IfStmt cond s) = p ("if " ++ show cond) >> p "then" >> prettyPrintStatement (ind+2) s
    format (WhileStmt cond s) = p ("while " ++ show cond) >> p "do" >> prettyPrintStatement (ind+2) s
    format PopBlock = p "return"
