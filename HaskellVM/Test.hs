module Main where

import Monad
import Control.Monad.State
import Control.Monad.Writer



type Z = Int
type Trace  = [String]
-- this is our combined monad type for this problem

type Controller z a = StateT (z, Trace) (Writer Trace) a

data Blob z = Blob z z Trace

getZ :: Blob z -> z
getZ (Blob _ z _) = z

getTrace :: Blob z -> Trace
getTrace (Blob _ _ trace) = trace

-- type Blob a =

-- type NDS a = WriterT [String] (StateT Z []) a



---tryC :: forall (t :: * -> *) s w a.
--        (Monoid w, MonadState s t) =>
--        StateT s (Writer w) a -> t (a, w)
-- tryC :: z -> Controller z a -> (Blob z, a) -- ((a, z), Trace)
tryC z0 controller = ((Blob z0 z1 trace), a)
    where ((a, z1), trace) = (runWriter (runStateT controller z0))


-- commit :: Blob z -> Controller z Bool
commit (Blob z0 z1 trace)
    = do zCur <- get
         (if (zCur == z0)
          then do put z1
                  tell trace
                  return True
          else do return False)
--tell trace b

test :: Controller Z ()
test = do
  tell ["Vorher"]

--   blob <- tryC testI z
--  commit blob

  tell ["Nachher"]


getZ :: Controller (Z, Trace) Z
getZ = gets fst

-- nur intern

putZ :: Z -> Controller (Z, Trace)
putZ z = do trace <- gets snd
            put (z, trace)

testI :: Controller Z ()
testI = do 
  (z, trace) <- getZ
  putZ (z+1)
  
  tellN ["Innen"]
  return ()


-- getSolution :: Controller Z a -> Z -> ((a,Trace), Z)
getSolution c z = (runWriter (evalStateT c z))

test1 = getSolution (testI) (10, [])

main = print test1

f :: Int -> Int
f n = (*) n
      n