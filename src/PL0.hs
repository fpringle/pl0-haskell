{-|
Module      : PL0
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

[PL/0](https://en.wikipedia.org/wiki/PL/0) is an educational programming language.
This package provides a library of datatypes and functions for tokenizing, lexing, parsing,
interpreting, and compiling (WIP) PL/0 programs.
-}

module PL0 (
  -- * Tokenizing and lexing
  module PL0.Token
  , module PL0.Lexer

  -- * Syntax and parsing
  , module PL0.Syntax
  , module PL0.Parser

  -- * Running a PL/0 program
  , module PL0.Interpreter
  ) where

import qualified PL0.Interpreter
import qualified PL0.Lexer
import qualified PL0.Parser
import qualified PL0.Syntax
import qualified PL0.Token
