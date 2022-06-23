module Compiler.Intermediate where

import qualified Data.Map as Map
import Data.List
import Data.Bifunctor
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

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

getBlockSymbols :: S.Block S.Identifier -> [ScopedIdentifier]
getBlockSymbols b =
  let
    consts    = map (topLevel . fst) $ S.constDecls b
    vars      = map topLevel $ S.varDecls b
    procNames = map (topLevel . fst) $ S.procDefs b
    procSyms  = map getProcSyms $ S.procDefs b

    getProcSyms :: (S.Identifier, S.Block S.Identifier) -> [ScopedIdentifier]
    getProcSyms (blockName, block) = map (addScope blockName) $ getBlockSymbols block
  in
    consts ++ vars ++ procNames ++ concat procSyms

getAllSymbols :: S.Program S.Identifier -> [ScopedIdentifier]
getAllSymbols (S.Program b) = getBlockSymbols b

mapSymbolsToAddresses :: [ScopedIdentifier] -> Map.Map ScopedIdentifier Address
mapSymbolsToAddresses ids = Map.fromList $ zip ids [0..]

filterScope :: [ScopedIdentifier] -> S.Identifier -> [ScopedIdentifier]
filterScope scopedIDs scope = filter isScope scopedIDs
  where
    isScope :: ScopedIdentifier -> Bool
    isScope (ScopedIdentifier (scope:rest) _) = True
    isScope _ = False

popScope :: [ScopedIdentifier] -> [ScopedIdentifier]
popScope = map helper
  where
    helper :: ScopedIdentifier -> ScopedIdentifier
    helper (ScopedIdentifier [] _) = error "tried to pop from top-level scop"
    helper (ScopedIdentifier (top:rest) id) = ScopedIdentifier rest id

transformStatement :: [ScopedIdentifier] -> [S.Identifier] -> S.Statement S.Identifier -> S.Statement ScopedIdentifier
transformStatement scopedIDs scope = fmap helper
  where
    helper id =
      let
        candidates = filter (\(ScopedIdentifier scopes oid) -> oid == id && isPrefixOf scopes (reverse scope)) scopedIDs
        nearest = maximumBy (comparing (\(ScopedIdentifier scopes _) -> length scopes)) candidates
        errmsg = "no candidates for id " ++ show id ++ "\ncandidates: " ++ show scopedIDs ++ "\ncurrent scope: " ++ show scope
      in
        if null candidates
        then error errmsg
        else nearest

transformBlock :: [ScopedIdentifier] -> S.Block S.Identifier -> S.Block ScopedIdentifier
transformBlock s = helper s s []
  where
    helper :: [ScopedIdentifier] -> [ScopedIdentifier] -> [S.Identifier] -> S.Block S.Identifier -> S.Block ScopedIdentifier
    helper scopedIDs origScopedIDs curScope block =
      let
        c = S.constDecls block
        v = S.varDecls block
        p = S.procDefs block
        b = S.body block
        newc = map (first topLevel) c
        newv = map topLevel v
        newp = map (\(id,f) -> (topLevel id, helper (popScope $ filterScope scopedIDs id) origScopedIDs (id : curScope) f)) p
        newb = transformStatement origScopedIDs curScope b
      in
        S.Block newc newv newp newb

transformSymbols :: [ScopedIdentifier] -> S.Program S.Identifier -> S.Program ScopedIdentifier
transformSymbols s (S.Program b) = S.Program $ transformBlock s b
