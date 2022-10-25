{-# OPTIONS_HADDOCK hide #-}
module PL0.Compiler.ThreeAddress where

import Numeric (showHex)
import Data.Char (toUpper)


type Index = Int
type Name = String

data Label =
  Named Name
  | Command Index
  deriving (Eq)

instance Show Label where
  show (Named name) = name
  show (Command idx) = show idx

data Address =
  Normal Int
  | Temporary Int
  deriving (Eq)

instance Show Address where
  show (Normal a) = "a" ++ show a
  show (Temporary t) = "t" ++ show t

data Value =
  Number Int
  | Addr Address
  deriving (Eq)

instance Show Value where
  show (Number n) = "0x" ++ showHex n ""
  show (Addr a) = show a

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
  | Odd Address Value
  | Marker Name
  | Print Value
  | Read Address
  | Call Name
  | Return
  deriving (Eq)

instance Show ThreeAddr where
  show (Move a v) = "MOV " ++ show a  ++ " " ++ show v
  show (Jnz v l) = "JNZ " ++ show v ++ " " ++ show l
  show (Arith a v1 op v2) = map toUpper (show op) ++ " " ++ show a ++ " " ++ show v1 ++ " " ++ show v2
  show (Compare a v1 cmp v2) = show cmp ++ " " ++ show a ++ " " ++ show v1 ++ " " ++ show v2
  show (Not a v) = "NOT " ++ show a ++ " " ++ show v
  show (Odd a v) = "ODD " ++ show a ++ " " ++ show v
  show (Marker name) = name ++ ":"
  show (Print v) = "PRINT " ++ show v
  show (Read a) = "READ " ++ show a
  show (Call name) = "CALL " ++ name
  show Return = "RET"
