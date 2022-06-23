module Compiler.Intermediate where

import qualified Data.Map as Map
import Data.List

import qualified Syntax as S
import Compiler.ThreeAddress


-- this module needs to turn S.Program -> Program

type Index = Int

data Program = Program {
  labels              :: Map.Map Label Index  -- maps labels to their index in "instructions"
  , dataRange         :: (Int, Int)
  } deriving (Show, Eq)

data ScopedIdentifier = ScopedIdentifier [String] S.Identifier
  deriving (Eq, Ord)

instance Show ScopedIdentifier where
  show (ScopedIdentifier scopes id) =
    intercalate "." (scopes ++ ["'" ++ id ++ "'"])

topLevel :: S.Identifier -> ScopedIdentifier
topLevel = ScopedIdentifier []

addScope :: String -> ScopedIdentifier -> ScopedIdentifier
addScope scope (ScopedIdentifier scopes id) = ScopedIdentifier (scope:scopes) id

getBlockSymbols :: S.Block -> [ScopedIdentifier]
getBlockSymbols b =
  let
    consts    = map (topLevel . fst) $ S.constDecls b
    vars      = map topLevel $ S.varDecls b
    procSyms  = map getProcSyms $ S.procDefs b

    getProcSyms :: (S.Identifier, S.Block) -> [ScopedIdentifier]
    getProcSyms (blockName, block) = map (addScope blockName) $ getBlockSymbols block
  in
    consts ++ vars ++ concat procSyms

getAllSymbols :: S.Program -> [ScopedIdentifier]
getAllSymbols (S.Program b) = getBlockSymbols b

mapSymbolsToAddresses :: [ScopedIdentifier] -> Map.Map ScopedIdentifier Address
mapSymbolsToAddresses ids = Map.fromList $ zip ids [0..]
