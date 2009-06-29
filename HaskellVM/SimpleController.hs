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

tryInput :: (Tick z) => Inp -> z -> (Outp)
tryInput inp z = traceOut . head . snd $ (runWriter (evalStateT (tick inp) z))

-- evalState (sequence . map tick $ inp)

{-
getVLin :: (Tick z) => z -> Vec
getVLin z = 
    let outs = tryInputs (replicate 2 (mkInp [])) z
        poss = map getPos outs
        l    = (fromIntegral $ length poss - 1 ) :: Dat
    in ((last poss) - (head poss)) * (l,l)
-}


calcV0Lin :: Pos -> Pos -> Vec
calcV0Lin pos0 pos1 = pos1-pos0

getVLin :: (Tick z) => Outp -> Controller z (Vec)
getVLin out = 
    do z <- get
       let pos1 = getPos $ tryInput (mkInp []) z
       return $ calcV0Lin (getPos out) pos1

 
calcV0 :: Pos -> Pos -> Vec
calcV0 pos0 pos1 = 
    let g = calcG pos0
    in (pos1 - pos0) - scale 0.5 g

getV :: (Tick z) => Outp -> Controller z (Vec)
getV out = 
    do z <- get
       let pos1 = getPos $ tryInput (mkInp []) z
       return $ calcV0 (getPos out) pos1


noop :: (Tick z) => Controller z Outp
noop = tick (mkInp [])


hohmann :: (Tick z) => Outp -> Dat -> Controller z (Outp)
hohmann out sollrad = 
    do v_  <- getVLin out
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
mytrace n l = trace (unlines $ L.intersperse " # " $ (n : map show l))

steuer :: (Tick z) => Outp -> Vec -> Controller z (Outp)
steuer out (vx,vy) = do
  -- TODO optimize here
  tick $ mkInp [(2,vx), (3,vy)] 
  

stayOnCircOrbit :: (Tick z) => Outp -> Dat -> Controller z (Outp)
stayOnCircOrbit out sollrad = 
    do v_  <- getVLin out
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
    do v_  <- getVLin out
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


getVTestController :: (Tick z) => Dat -> Controller z ()
getVTestController conf  = 
    do outm <- tick $ mkInp [(16000, 2001)]
       out0 <- noop
       linv0 <- getVLin out0
       v0_a  <- getV out0
       out1 <- noop
       let v0 = scale 0.5 ((getPos out1) - (getPos outm))
       let pos0 = getPos out0
           (pos1,v1) = calcTick (pos0, v0)
           (pos2,v2) = calcTick (pos1, v1)
       linv1 <- getVLin out1
       out2 <- noop
       mytrace "diffP 1,2:" [pos1- (getPos out1),pos2- (getPos out2)] $ return ()
       mytrace "diffv 1,2:" [linv0 - v0,linv1 - v1] $ return ()
       mytrace "v linv g p:" [v0,v0_a,linv0,normalize $ calcG pos0, normalize pos0] $ return ()
       
getVTestController2 :: (Tick z) => Dat -> Controller z ()
getVTestController2 conf  = 
    do tick $ mkInp [(16000, 2001)]
       out <- noop
       linv0 <- getVLin out
       v0   <- getV out
       let pos0 = getPos out
           (pos1,v1) = calcTick (pos0, v0)
           (pos2,v2) = calcTick (pos1, v1)
       out1 <- noop
       linv1 <- getVLin out
       out2 <- noop
       mytrace "diffP 1,2:" [pos1- (getPos out1),pos2- (getPos out2)] $ return ()
       mytrace "diffv 1,2:" [linv0 - v0,linv1 - v1] $ return ()
       mytrace "v linv g p:" [v0,linv0,normalize $ calcG pos0, normalize pos0] $ return ()
       
  
