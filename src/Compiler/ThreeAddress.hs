module Compiler.ThreeAddress where

type Label = String
type Address = Int

data Value =
  Number Int
  | Addr Address
  deriving (Show, Eq)

data Op =
  Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

data Comp =
  LT
  | LTE
  | GT
  | GTE
  | EQ
  | NE
  deriving (Show, Eq)

data ThreeAddr =
  Move Address Value
  | Jnz Value Label
  | Arith Address Value Op Value
  | Compare Address Value Comp Value
  | Not Address Value
  | Marker Label
  | Print Value
  | Read Address
  deriving (Show, Eq)
