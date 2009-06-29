module SimpleController where

import Types
import Controller
import ControllerUtils
import Util
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Debug.Trace
import qualified Data.List as L
import qualified Data.DList as DL


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
tryInputs inps z = map (\ (Trace1 _ _ x) -> x) . snd $ (runWriter (evalStateT (sequence . map tick $ inps) z))
--                   (evalState (runWriterT ) z) 
thd (_,_,c) = c
-- evalState (sequence . map tick $ inp)


getVLin :: (Tick z) => z -> Vec
getVLin z = 
    let outs = tryInputs (replicate 2 (mkInp [])) z
        poss = map getPos outs
        l    = (fromIntegral $ length poss - 1 ) :: Dat
    in ((last poss) - (head poss)) * (l,l)



{- 
getV :: (Tick z) => Pos -> z -> Vec
getV pos0 z = 
    let pos1 = head $ getPos (tryInputs (replicate 1 (mkInp [])) z)
        (pos0N, pos1N) = (normalize pos0, normalize pos1)
        (r02, r12) = (vecLen2 pos0, vecLen2 pos1)
        ag = mu * (1 / r02) * pos0N
        diff_g = ag / 2 * 1*1
    in ((last poss) - (head poss)) * (l,l)

-}

noop :: (Tick z) => Controller z Outp
noop = tick (mkInp [])


hohmann :: (Tick z) => Outp -> Dat -> Controller z (Outp)
hohmann out sollrad = 
    do v_  <- gets getVLin       
       let (v@(vx,vy)) = normalize v_
       let rad1 = getRad out           
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

mytrace :: (Show a) => String -> [a] -> b -> b
mytrace n l = trace (concat $ L.intersperse " # " $ (n : map show l))

steuer :: (Tick z) => Outp -> Vec -> Controller z (Outp)
steuer out (vx,vy) = do
  -- TODO optimize here
  tick $ mkInp [(2,vx), (3,vy)] 
  

stayOnCircOrbit :: (Tick z) => Outp -> Dat -> Controller z (Outp)
stayOnCircOrbit out sollrad = 
    do v_  <- gets getVLin
       let rad1   = getRad out     
           pos    = getPos out     
           sollv2 = vOnCirc sollrad
           raddiff = sollrad - rad1
           tangent  = normalize $ perpendicular pos
           tangent' = if scalar tangent v_ < 0 then negate tangent else tangent
           diff   = sollv2 - (vecLen v_)
           vdiff  = (scale sollv2 tangent') - (v_)           
--       mytrace "diffs (v,r):" [diff,raddiff] $ steuer out (scale (diff/1000) tangent')       
       mytrace "diffs (v):" [vdiff] $ steuer out (scale (0.1) vdiff)       
       mytrace "diffs (v,r):" [diff,raddiff] $ sequence_ $ replicate (1000) noop
       noop -- scheitelpunkt
    
stayOnCircOrbitRad :: (Tick z) => Outp -> Dat -> Controller z (Outp)
stayOnCircOrbitRad out sollrad = 
    do v_  <- gets getVLin
       let rad1   = getRad out     
           pos    = getPos out     
           sollv1 = vOnCirc rad1
           sollv2 = vOnCirc sollrad
           raddiff = sollrad - rad1
           tangent  = normalize $ perpendicular pos
           tangent' = if scalar tangent v_ < 0 then negate tangent else tangent
           diff   = sollv2 - (vecLen v_)
           vdiff  = (scale sollv2 tangent') - (v_)           
       mytrace "diffs (v,r):" [diff,raddiff] $ steuer out (scale (diff/1000) tangent')       
--       mytrace "diffs (v):" [vdiff] $ steuer out (scale (0.1) vdiff)       
       sequence_ $ replicate (10) noop
       noop -- scheitelpunkt
    

----------

task1Controller :: (Tick z) => Dat -> Controller z ()
task1Controller conf = 
    do out <- tick $ mkInp [(16000, conf)]
       let sollrad = getOut 4 out
       hohmann out sollrad 
       sequence_ $ replicate (1000) noop       
       return ()



task2Controller :: (Tick z) => Dat -> Controller z ()
task2Controller conf = 
    do out <- tick $ mkInp [(16000, conf)]
       z <- get
       let outp = tryInputs (repeat (mkInp [])) z
       trace (unlines $ map show $ take 100 outp) noop 
       return ()

georgController :: (Tick z) => Dat -> Controller z ()
georgController conf = 
    do out <- tick $ mkInp [(16000, conf)]
       z <- get
       let sollrad = (getRad out) * 1.2 -- test oribit
  --     mytrace "sollrad" [sollrad] $ hohmann out sollrad 
--       foldM (stayOnCircOrbit) out $ replicate 100 sollrad
--       L.foldl' (>>=) (return out) $ replicate 100 (flip stayOnCircOrbit sollrad)
       sequence_ $ replicate (20000) noop       
               
       return ()

{-
        goalPoint = (normalize $ -pos) * (sollrad,sollrad)
        cmds   = [(2,force1 * vx), (3,force1 * vy)] 
        params = defParams { thresh = 10, eps=0.1 }
        stat = optimizer2 params vm cmds (2,3) (optPoint goalPoint) (critTime (transtime+20)) getOptTimeMin
        (newcmds, _ , _, opttime) = stat
-}

