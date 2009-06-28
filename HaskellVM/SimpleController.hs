module SimpleController where

import Types
import Controller
import ControllerUtils
import Util
import Control.Monad.State.Strict
import Control.Monad.Writer


-- type NDS a = WriterT [String] (StateT Int []) a


--getSolution :: NDS a -> Int -> [((a,[String]), Int)]
--getSolution c i = (runStateT (runWriterT c) i)

--test1 = getSolution test 10


{-
-- type Trace m = m [(Inp, Outp)]
-- type Controller a z = WriterT Trace (State z) a

class Tick z where
    tick :: Inp -> Controller z
    getTime :: z   -> Time
-}


type Fahrplan = [(Time,Pos)]

-- ToDo calc eliptic stuff
-- getV :: (Tick z) => z -> Vec

tryInputs :: (Tick z) => [Inp] -> z -> ([Outp])
tryInputs inps z = fst $ (evalState (runWriterT (sequence . map tick $ inps)) z) 
-- evalState (sequence . map tick $ inp)


getVLin :: (Tick z) => z -> Vec
getVLin z = 
    let outs = tryInputs (replicate 2 (mkInp [])) z
        poss = map getPos outs
        l    = (fromIntegral $ length poss) :: Dat
    in ((last poss) - (head poss)) * (l,l)

noop :: (Tick z) => Controller z Outp
noop = tick (mkInp [])


-- simpleController :: forall t a. t -> IO [a]
simpleController :: (Tick z) => Dat -> Controller z ()
simpleController conf = 
    do out <- tick $ mkInp [(16000, conf)]
       v_  <- gets getVLin       
       let (v@(vx,vy)) = normalize v_
       let rad1 = getRad out
           sollrad = getOut 4 out
           pos     = getPos out
           transtime = fromIntegral $ floor $ hohmannTime rad1 sollrad :: Time
           force1 = - (hohmannSpeed1 rad1 sollrad)
           force2 = hohmannSpeed2 rad1 sollrad       
           goalPoint = (normalize $ -pos) * (sollrad, sollrad)
           cmds   = mkInp [(2,force1 * vx), (3,force1 * vy)] 
           cmds2  = mkInp [(2,force2 * vx), (3,force2 * vy)] 
       tick cmds
       sequence_ $ replicate (transtime-1) noop
       out <- noop -- scheitelpunkt
       tick cmds2
       sequence_ $ replicate (1000) noop       
       return ()
  

         
{-
        goalPoint = (normalize $ -pos) * (sollrad,sollrad)
        cmds   = [(2,force1 * vx), (3,force1 * vy)] 
        params = defParams { thresh = 10, eps=0.1 }
        stat = optimizer2 params vm cmds (2,3) (optPoint goalPoint) (critTime (transtime+20)) getOptTimeMin
        (newcmds, _ , _, opttime) = stat
-}

