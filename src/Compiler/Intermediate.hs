{-# LANGUAGE FlexibleInstances #-}
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

type SymbolMap = Map.Map ScopedIdentifier Address

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

mapSymbolsToAddresses :: [ScopedIdentifier] -> SymbolMap 
mapSymbolsToAddresses ids = Map.fromList $ zip ids $ map Normal [0..]

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

class TempEval a where
  -- _tempEval startAddr smap x returns a triple (cmds, newStartAddr) where
  --  startAddr is the first index of temporary space that the expression needs
  --    access to
  --  smap is a mapping of scoped symbols to permanent addresses
  --  x is the expression to evaluate
  --  cmds is the sequence of ThreeAddress commands that evaluates the expression
  --  newStartAddr is the next temp address not used by the expression
  --  address newStartAddr-1 contains the result of the expression
  _tempEval :: Int -> SymbolMap -> a -> ([ThreeAddr], Int)
  
  tempEval :: SymbolMap -> a -> ([ThreeAddr], Int)
  tempEval = _tempEval 0

tempV :: Int -> Value
tempV = Addr . Temporary

instance TempEval (S.Factor ScopedIdentifier) where
  _tempEval start smap (S.Ident id) = ([], start)
  _tempEval start smap (S.Num n) = ([Move (Temporary start) (Number n)], start+1)
  _tempEval start smap (S.Parens e) = _tempEval start smap e

instance TempEval (S.Term ScopedIdentifier) where
  _tempEval start smap (S.SingleFactor f) = _tempEval start smap f
  _tempEval start smap (S.Mul t f) =
    let
      (cmds1, ns1) = _tempEval start smap t
      (cmds2, ns2) = _tempEval ns1 smap f
      finalCmd = Arith (Temporary ns2) (tempV (ns1-1)) Mul (tempV (ns2-1))
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1)
  _tempEval start smap (S.Div t f) =
    let
      (cmds1, ns1) = _tempEval start smap t
      (cmds2, ns2) = _tempEval ns1 smap f
      finalCmd = Arith (Temporary ns2) (tempV (ns1-1)) Div (tempV (ns2-1))
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1)

instance TempEval (S.Expression ScopedIdentifier) where
  _tempEval start smap (S.UnaryPlus t) = _tempEval start smap t
  _tempEval start smap (S.UnaryMinus t) =
    let
      (cmds, ns) = _tempEval start smap t
      cmd = Arith (Temporary ns) (Number 0) Sub (tempV (ns-1))
    in
      (cmds ++ [cmd], ns+1)
  _tempEval start smap (S.BinaryPlus e t) =
    let
      (cmds1, ns1) = _tempEval start smap e
      (cmds2, ns2) = _tempEval ns1 smap t
      finalCmd = Arith (Temporary ns2) (tempV (ns1-1)) Add (tempV (ns2-1))
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1)

  _tempEval start smap (S.BinaryMinus e t) =
    let
      (cmds1, ns1) = _tempEval start smap e
      (cmds2, ns2) = _tempEval ns1 smap t
      finalCmd = Arith (Temporary ns2) (tempV (ns1-1)) Sub (tempV (ns2-1))
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1)
