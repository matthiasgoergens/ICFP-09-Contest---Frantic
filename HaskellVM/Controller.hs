{-# OPTIONS -XRankNTypes #-}
module Controller where
import Types
import Control.Monad.State.Strict
import Control.Monad.Writer

import VM
-- import Monad.List

type Trace  = [(Time, Inp, Outp)]
-- type Controller z m = z -> Trace m
type Controller z a = WriterT Trace (State z) a

-- oder anderer Rueckgabewert

runController :: (Dat -> Controller VM a)  -> VM -> Dat -> Trace
runController controller vm conf = snd . fst $ (runState (runWriterT (controller conf)) vm)


class Tick z where
    tick :: Inp -> Controller z Outp
    getTime :: z   -> Time

instance Tick VM where
    tick inp = do vm <- get
                  let (vm', outp) = oneRun inp vm
                  put vm'
                  tell [(time vm', inp, outp)] 
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

