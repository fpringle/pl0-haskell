module Compiler.Intermediate where

import qualified Syntax as S
import Compiler.ThreeAddress


-- this module needs to turn S.Program -> [ThreeAddr]

class UniqueSymbolStream a where
  nextSymbol :: a -> (Symbol, a)

instance UniqueSymbolStream Int where
  nextSymbol n = ("sym" ++ show n, n+1)

class ConvertEvaluate a where
  convertEvaluate :: UniqueSymbolStream s => s -> a -> ([ThreeAddr], Symbol, s)

instance ConvertEvaluate S.Expression where
  convertEvaluate s (S.UnaryPlus t) = convertEvaluate s t
  convertEvaluate s (S.UnaryMinus t) =
    let
      (start, inter, news) = convertEvaluate s t
      (finalSym, finalS) = nextSymbol news
      setInst = Arith finalSym (Number 0) Sub (Var inter)
    in
      (start ++ [setInst], finalSym, finalS)
  convertEvaluate s (S.BinaryPlus e t) =
    let
      (one, inter1, news) = convertEvaluate s e
      (lhsSym, nextS) = nextSymbol news
      (two, inter2, newnews) = convertEvaluate nextS t
      (rhsSym, nextnextS) = nextSymbol newnews
      (finalSym, finalS) = nextSymbol nextnextS
      setInst = Arith finalSym (Var lhsSym) Add (Var rhsSym)
    in
      (one ++ two ++ [setInst], finalSym, finalS)
  convertEvaluate s (S.BinaryMinus e t) =
    let
      (one, inter1, news) = convertEvaluate s e
      (lhsSym, nextS) = nextSymbol news
      (two, inter2, newnews) = convertEvaluate nextS t
      (rhsSym, nextnextS) = nextSymbol newnews
      (finalSym, finalS) = nextSymbol nextnextS
      setInst = Arith finalSym (Var lhsSym) Sub (Var rhsSym)
    in
      (one ++ two ++ [setInst], finalSym, finalS)

instance ConvertEvaluate S.Term where
  convertEvaluate s (S.SingleFactor f) = convertEvaluate s f
  convertEvaluate s (S.Mul t f) =
    let
      (one, inter1, news) = convertEvaluate s t
      (lhsSym, nextS) = nextSymbol news
      (two, inter2, newnews) = convertEvaluate nextS f
      (rhsSym, nextnextS) = nextSymbol newnews
      (finalSym, finalS) = nextSymbol nextnextS
      setInst = Arith finalSym (Var lhsSym) Mul (Var rhsSym)
    in
      (one ++ two ++ [setInst], finalSym, finalS)
  convertEvaluate s (S.Div t f) =
    let
      (one, inter1, news) = convertEvaluate s t
      (lhsSym, nextS) = nextSymbol news
      (two, inter2, newnews) = convertEvaluate nextS f
      (rhsSym, nextnextS) = nextSymbol newnews
      (finalSym, finalS) = nextSymbol nextnextS
      setInst = Arith finalSym (Var lhsSym) Div (Var rhsSym)
    in
      (one ++ two ++ [setInst], finalSym, finalS)

instance ConvertEvaluate S.Factor where
  convertEvaluate s (S.Ident id) = ([], id, s)
  convertEvaluate s (S.Num n) =
    let (sym, news) = nextSymbol s
    in ([Arith sym (Number n) Add (Number 0)], sym, news)
  convertEvaluate s (S.Parens e) = convertEvaluate s e

class Convert a where
  convert :: UniqueSymbolStream s => s -> a -> ([ThreeAddr], s)

instance Convert S.Statement where
  convert s (S.Set id e) =
    let
      (start, sym, news) = convertEvaluate s e
    in
      (start ++ [Arith id (Var sym) Add (Number 0)], news)
  convert s (S.Call id) = ([Jump Unconditional id], s)
  convert s (S.Input id) = ([Read id], s)
  convert s (S.Output e) =
    let
      (start, sym, news) = convertEvaluate s e
    in
      (start ++ [Print (Var sym)], news)
  convert s (S.StmtBlock []) = ([], s)
  convert s (S.StmtBlock (first:rest)) =
    let
      (start, news) = convert s first
      (insts, finals) = convert news (S.StmtBlock rest)
    in
      (start ++ insts, finals)
