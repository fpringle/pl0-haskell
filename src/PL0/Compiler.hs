{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
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

import PL0.Compiler.ThreeAddress
import PL0.Compiler.Intermediate
