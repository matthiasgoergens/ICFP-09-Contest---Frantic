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
           transtime = fromIntegral $ round $ hohmannTime rad1 sollrad :: Time
           force1 = - (hohmannSpeed1 rad1 sollrad)
           force2 = hohmannSpeed2 rad1 sollrad       
           goalPoint = (normalize $ -pos) * (sollrad, sollrad)
           cmds   = mkInp [(2,force1 * vx), (3,force1 * vy)] 
           cmds2  = mkInp [(2,force2 * vx), (3,force2 * vy)] 
       tick cmds
       sequence_ $ replicate (transtime-1) noop
       noop -- scheitelpunkt
       tick cmds2

hohmannEllipse :: (Tick z) => Outp -> Dat -> Controller z (Outp)
hohmannEllipse out sollrad = 
    do v_  <- getVLin out
       let (v@(vx,vy)) = normalize v_
       let rad1 = getRad out           
           pos     = getPos out
           transtime = fromIntegral $ round $ hohmannTime rad1 sollrad :: Time
           force1 = - (hohmannSpeed1 rad1 sollrad)
           cmds   = mkInp [(2,force1 * vx), (3,force1 * vy)] 
           cmds2  = mkInp [(2,-force1 * vx), (3,-force1 * vy)] 
       tick cmds
       sequence_ $ replicate (transtime*2-1) noop
       noop
       tick cmds2

mytrace :: (Show a) => String -> [a] -> b -> b
mytrace n l = trace (concat $ L.intersperse " # " $ (n : map show l))

steuer :: (Tick z) => Outp -> Vec -> Controller z (Outp)
steuer out (vx,vy) = do
  -- TODO optimize here
  tick $ mkInp [(2,vx), (3,vy)] 
  

tresh = 0.01
stayOnCircOrbit :: (Tick z) => Outp -> Dat -> Controller z (Outp)
stayOnCircOrbit out sollrad = 
    do v_  <- getV out
       let rad1   = getRad out     
           pos    = getPos out     
           sollv2 = vOnCirc sollrad
           raddiff = sollrad - rad1
           tangent  = normalize $ perpendicular pos
           tangent' = if scalar tangent v_ < 0 then negate tangent else tangent
           diff   = sollv2 - (vecLen v_)
--           vdiff  = (scale (sollv2) tangent') - (v_)      
           vdiff  = scale (diff) tangent'        
       if  abs(diff) < tresh then mytrace "noop" [raddiff] $ noop
            else mytrace "diffs (v,r) vd:" [(diff,raddiff),vdiff] $ steuer out (scale (0.0001) vdiff)       
       sequence_ $ replicate (10) noop
       noop

stayOnCircOrbit2 :: (Tick z) => Outp -> Dat -> Controller z (Outp)
stayOnCircOrbit2 out sollrad = 
    do hohmann out sollrad

        

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
       {-let pos  = -(getPos out)
           posO = (getPosOther out (4,5))
           phi1 = calcCircAng (-(getPos out))
           phi2 = calcCircAng (getPosOther out (4,5))
           phidiff = phi1-phi2                     
       out <- mytrace "pos" [pos, posO]$ noop
       out <- mytrace "phie" [phi1,phi2,phidiff]$ noop-}
       let sollrad = vecLen (getPosOther out (4,5))
       out <- mytrace "sollrad" [sollrad] $ hohmann out sollrad 
       let phi1 = calcCircAng (-(getPos out))
           phi2 = calcCircAng (getPosOther out (4,5))
           phidiff = toPhiRange $ phi1-phi2
           tau     = (timeOnCirc sollrad)
           tdiff   = tau * phidiff / (2 * pi) -- he is this time behind
           horad2  = hohmannTime1R2 sollrad ((tau-tdiff)/2)
       hohmannEllipse out horad2
--       foldM (stayOnCircOrbit) out $ replicate 2000 (sollrad)
--       foldM (stayOnCircOrbit2) out $ replicate 10 (sollrad)
--       L.foldl' (>>=) (return out) $ replicate 100 (flip stayOnCircOrbit sollrad)
       mytrace "phies" [phi1,phi2,phidiff]$ sequence_ $ replicate (6000) noop                   

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
       
  
