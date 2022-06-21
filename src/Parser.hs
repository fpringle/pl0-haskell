module Parser where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified Token as T
import qualified Syntax as S


data Parser = Parser {
  stack     :: [T.Token]
  , good    :: Bool
  }

type ParserT a = StateT Parser IO a

peek :: ParserT T.Token
peek = get >>= (return . head . stack)

empty :: ParserT Bool
empty = get >>= (return . (== 0) . length . stack)

pop :: ParserT T.Token
pop = do
  cur <- get
  let (hd:tl) = stack cur
  put $ cur { stack = tl }
  return hd

assertTopEq :: T.Token -> ParserT Bool
assertTopEq token = do
  top <- peek
  let res = top == token
  if not res
  then modify (\cur -> cur { good = False })
  else return ()
  return res

assertTopType :: T.Token -> ParserT Bool
assertTopType token = do
  top <- peek
  let res = T.sameType top token
  if not res
  then modify (\cur -> cur { good = False })
  else return ()
  return res
