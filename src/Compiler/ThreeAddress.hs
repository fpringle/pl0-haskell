module Compiler.ThreeAddress where

type Symbol = String
type Label = String

data Value =
  Number Int
  | Var Symbol

data Op =
  Add
  | Sub
  | Mul
  | Div

data Comp =
  LT
  | LTE
  | GT
  | GTE
  | EQ
  | NE

data Condition = 
  Unconditional
  | Compare Value Comp Value

data ThreeAddr =
  Arith Symbol Value Op Value       -- s1 = s2 OP s3
  | Jump Condition Label            -- if cond, jump to label
  | Marker Label                    -- label:
  | Print Value                     -- write value
  | Read Symbol                     -- sym = read
