module Interpreter where

import qualified Data.Map as Map

import Syntax


type IdMap a = Map.Map Identifier a

data SymbolTable = SymbolTable {
  constants         :: IdMap Int
  , variables       :: IdMap (Maybe Int)
  , functions       :: IdMap Block
  }

data Scope = Scope {
  tables    :: [SymbolTable]
  , stmt    :: Statement
  }

data Interpreter = Interpreter {
  scope     :: Scope
  }
