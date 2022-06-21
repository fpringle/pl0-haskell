module Syntax where


type Identifier = String
type Number = Int

-- syntax

data Expression =
  UnaryPlus Term
  | UnaryMinus Term
  | BinaryPlus Expression Term
  | BinaryMinus Expression Term
  deriving (Show, Eq)

data Term =
  SingleFactor Factor
  | Mul Term Factor
  | Div Term Factor
  deriving (Show, Eq)

data Factor =
  Ident Identifier
  | Num Number
  | Parens Expression
  deriving (Show, Eq)

data Op =
  OP_LT
  | OP_LTE
  | OP_GT
  | OP_GTE
  | OP_EQ
  | OP_HASH
  deriving (Show, Eq)

data Condition =
  Odd Expression
  | Comp Expression Op Expression
  deriving (Show, Eq)

data Statement =
  Set Identifier Expression
  | Call Identifier
  | Input Identifier
  | Output Expression
  | StmtBlock [Statement]
  | IfStmt Condition Statement
  | WhileStmt Condition Statement
  deriving (Show, Eq)

data Block = Block {
  constDecls    :: [(Identifier, Number)]
  , varDecls    :: [Identifier]
  , procDefs    :: [(Identifier, Block)]
  , body        :: Statement
  } deriving (Show, Eq)

data Program = Program Block
  deriving (Show, Eq)
