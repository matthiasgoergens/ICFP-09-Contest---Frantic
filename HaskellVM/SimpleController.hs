module SimpleController where

import Types
import Controller
import ControllerUtils
import Util
import Debug.Trace
import Data.Function
import Control.Monad.State.Strict
import qualified Data.List as L
import qualified Data.DList as DL


import GradientDescent

  
-- does not work because it oscillates
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
---- CONTROLLER FOR TASKS

task1Controller' :: (Tick z) => Dat -> Controller z ()
task1Controller' conf = 
    do out <- tick $ mkInp [(16000, conf)]
       let sollrad = getOut 4 out
       hohmann out sollrad 
       sequence_ $ replicate (1000) noop       
       return ()

-- Task 1 with fuel burning
task1Controller :: (Tick z) => Dat -> Controller z ()
task1Controller conf = 
    do out <- tick $ mkInp [(16000, conf)]
       let sollrad = getOut 4 out
       out <- hohmann out sollrad 
       v   <- getVLin out
       let sprit = (getOut 1 out)
       out <- steuer out (scale (sprit/4) (normalize v))
       out <- steuer out (scale (-sprit/2) (normalize v))
       out <- steuer out (scale ((getOut 1 out)-0.000000000001) (normalize v))
       sequence_ $ replicate (1000) noop       
       return ()



task2Controller :: (Tick z) => Dat -> Controller z ()
task2Controller conf = 
    do out <- tick $ mkInp [(16000, conf)]
       let pos  = -(getPos out)
           posO = (getPosOther (4,5) out)
           phi1 = calcCircAng (-(getPos out))
           phi2 = calcCircAng (getPosOther (4,5) out )
           phidiff = phi1-phi2                     
       out <- mytrace "pos" [pos, posO]$ noop
       out <- mytrace "phie" [phi1,phi2,phidiff]$ noop
       let sollrad = vecLen (getPosOther (4,5) out)
       out <- mytrace "sollrad" [sollrad] $ hohmann out sollrad 
       let phi1 = calcCircAng (-(getPos out))
           phi2 = calcCircAng (getPosOther (4,5) out )
           phidiff = toPhiRange $ phi1-phi2
           tau     = (timeOnCirc sollrad)
           tdiff   = tau * phidiff / (2 * pi) -- he is this time behind
           horad2  = hohmannTime1R2 sollrad ((tau-tdiff)/2)
       hohmannEllipse out horad2
       out <- noop
       foldM (follow 0.0) out $ replicate 1000 ((4,5))
       
--       foldM (stayOnCircOrbit) out $ replicate 2000 (sollrad)
--       foldM (stayOnCircOrbit2) out $ replicate 10 (sollrad)
--       L.foldl' (>>=) (return out) $ replicate 100 (flip stayOnCircOrbit sollrad)
       mytrace "phies" [phi1,phi2,phidiff]$ sequence_ $ replicate (6000) noop                   

       return ()


task3Controller :: (Tick z) => Dat -> Controller z ()
task3Controller conf = 
    do out <- tick $ mkInp [(16000, conf)]
       z <- get
       let tryout = tryInputs (replicate 10000 (mkInp [])) z
           possO  = map (getPosOther (4,5)) tryout           
           (maxT,maxP) = (L.maximumBy (compare `on` vecLen . snd) $ zip[1..] possO) :: (Time,Vec)
           minP   = L.minimumBy (compare `on` vecLen) possO
       let rad     = getRad out
           sollrad = vecLen (maxP)
           sollrad2 = vecLen (minP)
           outT     = round $ hohmannTime rad sollrad
           diffT    = (maxT+1) - outT           
           cycleTime= timeOnEllipse $ vecLen (maxP - minP)/2
           rad3     = (vecLen minP)
           -- rad2 is the intermediate radius to spend the time he spends on the ellipse
           (rad2,qual)= optimize1 (defParams {eps = 1, maxiter = 1000})  ( (< 0.1) . abs) 
                      (\ r2 -> (hohmannTime rad r2 + hohmannTime r2 rad3) - cycleTime) rad 
       (out,force1) <- mytrace "sollrad" [rad2, qual] $ hohmannNoEnd out rad2 (0,0)
       (out,force2) <- hohmannNoEnd out rad3 force1
       force3       <- hohmannGetForce1 out (vecLen maxP)
       out <- steuer out (force2+force3)       
       z   <- get
       out <- mytrace "time" [getTime z]  noop
       -- go to him in 10 steps ;-)
       out <- steuerToHim out (4,5) 10
       -- follow his path ;-)
       out <- steuerAlongHim out (4,5)
       sequence_ $ replicate (1000) noop                   
       foldM (follow 0.0) out $ replicate 10 ((4,5))
       return ()

doWhile :: (Tick z) => ((Outp,Outp) -> Bool) -> Outp -> Time -> Controller z (Outp,Time)
doWhile crit out 0 = return (out,0)
doWhile crit out maxsteps = 
    do out2 <- noop
       if crit (out,out2) then doWhile crit out (maxsteps -1)
          else return (out2, maxsteps)
--    do outpairs <- liftM (\x -> takeWhile crit $ zip x $ drop 1 x) (sequence (replicate maxsteps noop))
--       return ((fst . head) outpairs, length outpairs)

getCloser :: (Outp,Outp) -> Bool
getCloser (o1,o2) = 
    let pos1     = -(getPos o1)
        pos1O    = getPosOther (4,5) o1
        d1       = vecLen (pos1O - pos1)
        pos2     = -(getPos o2)
        pos2O   = getPosOther (4,5) o2
        d2       = vecLen (pos2O - pos2)
        in d1 > d2


-- try to reach him in 1000 steps, then go closer in 100 and then make small adjustments
stupidController :: (Tick z) => Dat -> Controller z ()
stupidController conf = 
    do out <- tick $ mkInp [(16000, conf-2000)]
       z <- get
       let t1 = 1000
       let t2 = 100
       let tryout  = tryInputs (replicate t1 (mkInp [])) z
           pos     = -(getPos out)
           possO   = map (getPosOther (4,5)) tryout                                          
           goalpos = last possO
       out <- steuerToPoint out (goalpos) (length tryout)
       out <- steuerAlongHim out (4,5)
       z <- get
       let tryout  = tryInputs (replicate t2 (mkInp [])) z
           pos     = -(getPos out)
           possO   = map (getPosOther (4,5)) tryout                                          
           goalpos = last possO
       out <- steuerToPoint out (goalpos) (length tryout)
       out <- steuerAlongHim out (4,5)
       out <- steuerToHim out (4,5) 10
       -- follow his path ;-)
       out <- steuerAlongHim out (4,5)
       foldM (follow 0.0) out $ replicate 2 ((4,5))
       sequence_ $ replicate (1000) noop                   
       foldM (follow 0.0) out $ replicate 2 ((4,5))
       return ()

noopController :: (Tick z) => Time -> Dat -> Controller z ()
noopController maxtime conf = 
    do out <- tick $ mkInp [(16000, conf-4000)]
       sequence_ $ replicate (maxtime-1) noop
       return ()


testHohmannController :: (Tick z) => Dat -> Controller z ()
testHohmannController conf = 
    do out <- tick $ mkInp [(16000, conf)]
       let sollrad = vecLen (getPosOther (4,5) out)
       out <- mytrace "sollrad" [sollrad] $ hohmann out sollrad 
       sequence_ $ replicate (300000) noop                   

getVTestController :: (Tick z) => Dat -> Controller z ()
getVTestController conf  = 
    do outm <- tick $ mkInp [(16000, 1001)]
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

------- TESTING
       
n = 1000

getVTestController2 :: (Tick z) => Dat -> Controller z ()
getVTestController2 _  =
    do tick $ mkInp [(16000, 2003)]

       outs <- wait (n+1)

       let poss@(pos0:pos1:_) = map (getPos) outs

           v0 = calcV0 pos0 pos1

           virts = map fst $ iterate calcTick $ (pos0, v0)

--       trace ("Pos Real1:\t"++ show pos1) $ noop
--       trace ("Pos Virt1:\t"++ show pos1') $ noop

       trace ("Pos Real0:\t"++ show (poss !! 0)) $ noop
       trace ("Pos Virt0:\t"++ show (virts !! 0)) $ noop

       trace ("Pos diff0:\t"++ show ((poss !! 0)-(virts !! 0))) $ noop

       trace ("Pos Realn:\t"++ show (poss !! n)) $ noop
       trace ("Pos Virtn:\t"++ show (virts !! n)) $ noop

       trace ("Pos diffn:\t"++ show ((poss !! n)-(virts !! n))) $ noop

       return ()

{-       trace ("Pos Virt2:\t"++ show pos2) $ noop
       trace ("Pos Real2:\t"++ show (getPos out2)) $ noop

       trace ("V Virt1:\t"++ show v1) $ noop
       trace ("V Real1:\t"++ show linv1) $ noop

       trace ("V Virt2:\t"++ show v2) $ noop
       trace ("V Real2:\t"++ show linv2) $ noop

-}

{-
       trace (show (getPos out)) getNextPosNoop

       linv0 <- getVLin out

       v0   <- getV out

       let pos0 = getPos out
           (pos1,v1) = calcTick (pos0, v0)
           (pos2,v2) = calcTick (pos1, v1)

       out1 <- (noop)

       linv1 <- getVLin out
       out2 <- noop
       linv2 <- getVLin out



       return ()
--       mytrace "diffv 1,2:\t" [linv0 - v0,linv1 - v1] $ return ()
--       mytrace "v linv g p:" [v0,linv0,normalize $ calcG pos0, normalize pos0] $ return ()



-}
