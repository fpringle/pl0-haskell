{-# LANGUAGE FlexibleInstances #-}
module Compiler.Intermediate where

import Prelude hiding (LT, GT, EQ)
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

type SymbolMap = Map.Map ScopedIdentifier Int

-- analyse symbols in scopes
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


-- evaluate expressions etc
class TempEval a where
  -- _tempEval startAddr smap x returns a triple (cmds, newStartAddr, resultAddr) where
  --  startAddr is the first index of temporary space that the expression needs
  --    access to
  --  smap is a mapping of scoped symbols to permanent addresses
  --  x is the expression to evaluate
  --  cmds is the sequence of ThreeAddress commands that evaluates the expression
  --  newStartAddr is the next temp address not used by the expression
  --  address resultAddr contains the result of the expression
  _tempEval :: Int -> SymbolMap -> a -> ([ThreeAddr], Int, Value)
  
  tempEval :: SymbolMap -> a -> ([ThreeAddr], Int, Value)
  tempEval = _tempEval 0

tempV :: Int -> Value
tempV = Addr . Temporary

normV :: Int -> Value
normV = Addr . Normal

noSym :: ([ThreeAddr], Int) -> ([ThreeAddr], Int, Value)
noSym (ta, ns) = (ta, ns, tempV ns)

instance TempEval (S.Factor ScopedIdentifier) where
  _tempEval start smap (S.Ident id) = 
    case Map.lookup id smap of
      Just addr -> ([], start, normV addr)
      Nothing   -> error ("unknown symbol: " ++ show id)
  _tempEval start smap (S.Num n) = ([], start, Number n)
  _tempEval start smap (S.Parens e) = _tempEval start smap e

instance TempEval (S.Term ScopedIdentifier) where
  _tempEval start smap (S.SingleFactor (S.Ident id)) = error "this should never have been called (_tempEval _ _ (S.SingleFactor S.Ident id))"
  _tempEval start smap (S.SingleFactor f) = _tempEval start smap f

  _tempEval start smap (S.Mul t f) =
    let
      (cmds1, ns1, res1) = _tempEval start smap t
      (cmds2, ns2, res2) = _tempEval ns1 smap f
      finalCmd = Arith (Temporary ns2) res1 Mul res2
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1, tempV ns2)

  _tempEval start smap (S.Div t f) =
    let
      (cmds1, ns1, res1) = _tempEval start smap t
      (cmds2, ns2, res2) = _tempEval ns1 smap f
      finalCmd = Arith (Temporary ns2) res1 Div res2
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1, tempV ns2)

instance TempEval (S.Expression ScopedIdentifier) where
  _tempEval start smap (S.UnaryPlus t) = _tempEval start smap t

  _tempEval start smap (S.UnaryMinus t) =
    let
      (cmds, ns, res) = _tempEval start smap t
      cmd = Arith (Temporary ns) (Number 0) Sub res
    in
      (cmds ++ [cmd], ns+1, tempV ns)

  _tempEval start smap (S.BinaryPlus e t) =
    let
      (cmds1, ns1, res1) = _tempEval start smap e
      (cmds2, ns2, res2) = _tempEval ns1 smap t
      finalCmd = Arith (Temporary ns2) res1 Add res2
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1, tempV ns2)

  _tempEval start smap (S.BinaryMinus e t) =
    let
      (cmds1, ns1, res1) = _tempEval start smap e
      (cmds2, ns2, res2) = _tempEval ns1 smap t
      finalCmd = Arith (Temporary ns2) res1 Sub res2
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1, tempV ns2)

instance TempEval (S.Condition ScopedIdentifier) where
  _tempEval start smap (S.Odd e) =
    let
      (cmds, ns, res) = _tempEval start smap e
      finalCmd = Odd (Temporary ns) res
    in
      (cmds ++ [finalCmd], ns+1, tempV ns)

  _tempEval start smap (S.Comp e1 op e2) =
    let
      (cmds1, ns1, res1) = _tempEval start smap e1
      (cmds2, ns2, res2) = _tempEval ns1 smap e2
      finalCmd = Compare (Temporary ns2) res1 (convOp op) res2
    in
      (cmds1 ++ cmds2 ++ [finalCmd], ns2+1, tempV ns2)
    where
      convOp :: S.Op -> Comp
      convOp S.OP_LT = LT
      convOp S.OP_LTE = LTE
      convOp S.OP_GT = GT
      convOp S.OP_GTE = GTE
      convOp S.OP_EQ = EQ
      convOp S.OP_HASH = NE


