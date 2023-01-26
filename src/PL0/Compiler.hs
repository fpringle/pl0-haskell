{-|
Module      : PL0.Compiler
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

Compile PL/0 to machine code.
-}

{-# OPTIONS_HADDOCK hide #-}

module PL0.Compiler
  (
    getAllSymbols
    , transformBlock
    , mapSymbolsToAddresses
    , tempEval
    , eval
  ) where

import PL0.Compiler.Intermediate
import PL0.Compiler.ThreeAddress
