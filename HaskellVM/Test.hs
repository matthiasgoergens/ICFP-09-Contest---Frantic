module Main where

import Monad
import Control.Monad.State
import Control.Monad.Writer


-- this is our combined monad type for this problem
type NDS a = WriterT [String] (State Int) a

-- type NDS a = WriterT [String] (StateT Int []) a

test :: NDS Bool
test = do 
  i <- get 
  put (i+1)
  tell ["A"]
  tell ["B", "C"]
  return False


getSolution :: NDS a -> Int -> ((a,[String]), Int)
getSolution c i = (runState (runWriterT c) i)

test1 = getSolution test 10

main = print test1