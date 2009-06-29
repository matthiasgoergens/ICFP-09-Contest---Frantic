{-# OPTIONS -XRankNTypes #-}
module Controller where
import Types
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
import qualified Data.DList as DL

import VM
-- import Monad.List

type Trace  = DL.DList Trace1
data Trace1  = Trace1 !Time !Inp !Outp

traceOut :: Trace1 -> Outp
traceOut (Trace1 _ _ o) = o

--type Controller z a = StateT z (Writer Trace) a
type Controller z a = WriterT Trace (State z) a

-- oder anderer Rueckgabewert

runController :: (Controller VM a)  -> VM -> Trace
runController controller vm = snd . fst $ (runState (runWriterT (controller)) vm)
                              -- snd $ (runWriter (runStateT (controller) vm))

-- runController2 :: (Controller VM a)  -> VM -> VM
runController2 controller vm = -- snd . fst $ (runState (runWriterT (controller)) vm)
                              (runWriter (runStateT (controller) vm))


class Tick z where
    tick :: Inp -> Controller z Outp
    getTime :: z -> Time

instance Tick VM where
    tick inp = do vm <- get
                  let (vm', outp) = oneRun inp vm
                  put vm'
                  tell $! DL.singleton $ Trace1 (time vm) inp outp 
                  return outp
    getTime (VM {time =t}) = t

-- doStep :: Tick z => Inp -> State (Outp, z) ()
-- doStep inp = get >>= put . runState (tick inp) . snd
                     
-- Welcher Typ ist praktischer?

-- tryStep :: Tick z => Inp -> State (Outp, z) (Outp, z)
-- tryStep inp = get >>= return . runState (tick inp) . snd

-- tryStep :: Tick z => Inp -> z -> (Outp, z)
-- tryStep inp z = runState (tick inp) z


-- oneRun :: Inp -> VM -> (VM, Outp)

