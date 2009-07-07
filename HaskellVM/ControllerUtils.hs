module ControllerUtils where

import Types
import Util
import Controller

import qualified Data.IntMap as I
import qualified Data.DList as DL
import qualified Data.List as L
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
import Debug.Trace
import Data.Function
import Test.QuickCheck


mytrace :: (Show a) => String -> [a] -> b -> b
mytrace n l = trace (concat $ L.intersperse " # " $ (n : map show l))


g = 6.67428E-11
m_e = 6e24

mu = let g = 6.67428E-11
         m_e = 6e24
     in --m_e * g;
       g * m_e;

hohmannSpeed1 :: Dat -> Dat -> Dat
hohmannSpeed1 r1 r2 = 
    sqrt(mu / r1) * (sqrt (2 * r2 / (r1 + r2)) - 1)


hohmannSpeed2 :: Dat -> Dat -> Dat
hohmannSpeed2 r1 r2 = 
    sqrt(mu / r2) * (1 - sqrt(2 * r1 / (r1 + r2)))

hohmannTime :: Dat -> Dat -> Dat
hohmannTime r1 r2 = pi * sqrt ((r1 + r2)^3 / (8 * mu)) 

hohmannTime1R2 :: Dat -> Dat -> Dat
hohmannTime1R2 r1 th = ((8 * mu)**(1/3)) * ((th/pi)**(2/3)) - r1;

calcCircAng :: Pos -> Dat
calcCircAng (x,y) = atan2 y x

-- shifts phi into [-pi,pi]
toPhiRange :: Dat -> Dat
toPhiRange phi | phi < -pi = phi + 2*pi 
               | phi >  pi = phi - 2*pi 
               | otherwise = phi

--- fliehkraft = Gravitation 
--   m_s v^2 /r = mu m_s / r^2
vOnCirc :: Dat -> Dat
vOnCirc r =  sqrt (mu/r)

fliehkraft :: Dat -> Dat -> Dat
fliehkraft v r = v^2 / r

-- time for one orbit = 2pi/omega = 2pi*r/v
timeOnCirc :: Dat -> Dat
timeOnCirc r =  2*pi*r / (vOnCirc r)

-- time for one ellipse = 2pi/omega = 2pi*r/v
timeOnEllipse :: Dat -> Dat
timeOnEllipse a =  sqrt ( 4*pi*pi*(a*a*a) / mu)

calcRadialVelocity :: Pos -> Vec -> Dat
calcRadialVelocity pos v = 
    let tangent  = normalize $ perpendicular pos
        radvel   = scalar tangent v
    in if radvel < 0 then -radvel else radvel

calcTick :: (Pos, Vec) -> (Pos,Vec)
calcTick (pos0,v0) = 
    let pos1 = pos0 + v0 + (scale 0.5 (calcG pos0))
        v1 = v0 + scale 0.5 (calcG pos0 + calcG pos1)
    in (pos1, v1)

{- calcG (0,0) = error "Die Reise zum Mittelpunkt der Erde!"
calcG pos@(x,y) = -- scale (mu / r2) (normalize pos)
                  (mu / r15 * x, mu / r15 * y)
    where normalize v@(x,y) = let len = (1/vecLen v)
                              in (x*len,y*len)

          r = vecLen pos
          r2 = r * r
          r15 = r2 * r
-}

calcG pos = scale (mu / (vecLen2 pos)) $ normalize pos



calcV0 :: Pos -> Pos -> Vec
calcV0 pos0 pos1 = 
    let g = calcG pos0
    in (pos1 - pos0) - scale 0.5 g


calcV0Lin :: Pos -> Pos -> Vec
calcV0Lin pos0 pos1 = pos1-pos0


-- calcTick und calcV0 sollten invers sein
prop_calcTick_calcV0 (pos0, pos1) = ((vecLen2 pos0 > 0)
                                     && (vecLen2 pos1 > 0)
                                     && (vecLen2 (pos1 - pos0) > 0))
                                    ==> (diff < 0.001)
    where v0 = calcV0 pos0 pos1
          types :: (Pos, Pos)
          types = (pos0, pos1)
          (pos1a, v1) = (calcTick (pos0, v0))
          diff = vecLen2 (pos1 - pos1a)

prop_calcTick_calcV0_Inv (pos0, v0) = ((vecLen2 pos0 > 0)
                                     && (vecLen2 v0 > 0)
                                     && (vecLen2 (pos1 - pos0) > 0))
                                    ==> (diff < 0.001)
    where (pos1, v1) = (calcTick (pos0, v0))
          v0a = calcV0 pos0 pos1
          diff = vecLen2 (v0 - v0a)

          types :: (Pos, Pos)
          types = (pos0, pos1)

prop_calcG pos = (vecLen2 pos > 0)
                 ==> (not . prop $ calcG pos)
    where prop (gx,gy) = ((||) `on` isNaN) gx gy



-----------

tryInputs :: (Tick z) => [Inp] -> z -> ([Outp])
-- tryInputs inps z = map (\ (Trace1 _ _ x) -> x) . DL.toList . snd $ (runWriter (evalStateT (sequence . map tick $ inps) z))
tryInputs inps z = map (\ (Trace1 _ _ x) -> x) . DL.toList . snd $
                   (evalState (runWriterT (sequence . map tick $ inps)) z)

tryInput :: (Tick z) => Inp -> z -> (Outp)
tryInput inp z = traceOut . DL.head . snd $ (evalState  (runWriterT (tick inp)) z)
-- tryInput inp z = traceOut . DL.head . snd $ (runWriter (evalStateT (tick inp) z))

{-
getVLin :: (Tick z) => z -> Vec
getVLin z = 
    let outs = tryInputs (replicate 2 (mkInp [])) z
        poss = map getPos outs
        l    = (fromIntegral $ length poss - 1 ) :: Dat
    in ((last poss) - (head poss)) * (l,l)
-}


getNextPosNoop :: (Tick z) => Controller z Pos
getNextPosNoop = do
  z <- get
  return (getPos $ tryInput (mkInp []) z)

getVLin :: (Tick z) => Outp -> Controller z (Vec)
getVLin out = 
    do pos1 <- getNextPosNoop
       return $ calcV0Lin (getPos out) pos1

 
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
           steuer1 = (force1 * vx, force1 * vy)
           steuer2 = (force2 * vx, force2 * vy)
       steuer out steuer1
       sequence_ $ replicate (transtime-1) noop
       outp <- noop -- scheitelpunkt
       steuer out steuer2

-- only half round and tells what force to use
hohmannNoEnd :: (Tick z) => Outp -> Dat -> (Vec) -> Controller z (Outp,Vec)
hohmannNoEnd out sollrad initforce = 
    do v_  <- getVLin out
       let (v@(vx,vy)) = normalize v_
       let rad1 = getRad out           
           pos     = getPos out
           transtime = fromIntegral $ round $ hohmannTime rad1 sollrad :: Time
           force1 = - (hohmannSpeed1 rad1 sollrad)
           force2 = hohmannSpeed2 rad1 sollrad       
           goalPoint = (normalize $ -pos) * (sollrad, sollrad)
           steuer1 = (force1 * vx, force1 * vy)
           steuer2 = (force2 * vx, force2 * vy)
       steuer out (steuer1 + initforce)
       sequence_ $ replicate (transtime-1) noop
       outp <- noop -- scheitelpunkt
       return (outp, steuer2)

-- uses new end 
hohmannGetForce1 :: (Tick z) => Outp -> Dat -> Controller z (Vec)
hohmannGetForce1 out sollrad = 
    do v_  <- getVLin out
       let (v@(vx,vy)) = normalize v_
       let rad1 = getRad out           
           pos     = getPos out
           transtime = fromIntegral $ round $ hohmannTime rad1 sollrad :: Time
           force1 = - (hohmannSpeed1 rad1 sollrad)
           steuer1 = (force1 * vx, force1 * vy)
       return (steuer1)


hohmannEllipse :: (Tick z) => Outp -> Dat -> Controller z (Outp)
hohmannEllipse out sollrad = 
    do v_  <- getVLin out
       let (v@(vx,vy)) = normalize v_
       let rad1 = getRad out           
           pos     = getPos out
           transtime = fromIntegral $ round $ hohmannTime rad1 sollrad :: Time
           force1 = - (hohmannSpeed1 rad1 sollrad)
           steuer1 = (force1 * vx, force1 * vy)
           steuer2 = (-force1 * vx, -force1 * vy)
       steuer out steuer1
       sequence_ $ replicate (transtime*2-1) noop
       outp <- noop -- scheitelpunkt
       steuer out steuer2


steuer :: (Tick z) => Outp -> Vec -> Controller z (Outp)
steuer outp (vx,vy) = do  
  -- TODO optimize here
  tick $ mkInp [(2,vx), (3,vy)] 

steuerWithCentrifComp :: (Tick z) => Outp -> Vec -> Controller z (Outp)
steuerWithCentrifComp outp action = do  
  v <- getVLin outp
  let pos  = -(getPos outp)            
      f1    = fliehkraft (calcRadialVelocity pos v) (getRad (outp))
      pos2  = pos + v+action
      f2    = fliehkraft (calcRadialVelocity pos2 (v+action)) (getRad (outp))
      action' = action + (scale ((f2-f1)) $ normalize pos)
  steuer outp (scale (1) action')

-- Tries to reach the point in n steps with Gravity and Centrifugal compensation
steuerToPoint :: (Tick z) => Outp -> Pos -> Time -> Controller z (Outp)
steuerToPoint outp goalpos steps = do  
  z <- get
  v <- getVLin outp
  let tryout = tryInputs (replicate steps (mkInp [])) z
      pos    = -(getPos outp)
      (posf:posf1:_)  = take 2 $ reverse $ map (negate . getPos) tryout       
      action = (goalpos - posf)
      scaled = (scale (1.0/(fromIntegral steps)) action)
      v2     = v + scaled
      f1    = fliehkraft (calcRadialVelocity pos v) (getRad (outp))      
      f2    = fliehkraft (calcRadialVelocity posf (v2)) (getRad (outp))
      g1    = calcG pos
      g2    = calcG goalpos
      action' = scale (1.0/(fromIntegral steps)) (action + ((scale (-f1)) $ normalize pos)
                                                  + ((scale (f2)) $ normalize goalpos)
                                                  - g1 + g2)
  outp <- mytrace "distance: " [action] $ steuer outp action'
  outp <- waitGetLast (steps) outp
  return outp


steuerToHim :: (Tick z) => Outp -> (Addr,Addr) -> Time -> Controller z (Outp)
steuerToHim outp other steps = do  
  v <- getVLin outp
  let pos  = -(getPos outp)
      opos = getPosOther other outp
      action = opos - pos
      scaled = (scale (1.0/fromIntegral steps) action)
  outp <- mytrace "distance: " [action] $  steuerWithCentrifComp outp scaled
  outp <- waitGetLast (steps-1) outp
  outp <- steuerWithCentrifComp outp (scale (-1) scaled)
  return outp
            
steuerAlongHim :: (Tick z) => Outp -> (Addr,Addr) -> Controller z (Outp)
steuerAlongHim outp other  = do  
  z <- get
  let nextout = tryInput (mkInp []) z
  let pos  = -(getPos outp)
      opos = getPosOther other outp
      pos2  = -(getPos nextout)
      opos2 = getPosOther other nextout
      v     = pos2-pos
      ov    = opos2-opos
      action = ov - v
  outp <- mytrace "v, hisv" [v, ov] $ steuerWithCentrifComp outp action
  return outp 


follow :: (Tick z) => Double -> Outp -> (Addr,Addr) -> Controller z (Outp)
follow strength out other = 
    do let pos    = -(getPos out)
           opos   = getPosOther other out
           pdiff = opos - pos           
       mytrace "pdiff" [pdiff] $ steuer out (scale (strength) pdiff)

wait :: Tick z => Int -> Controller z [Outp]
wait n = sequence $ replicate n noop

waitGetLast :: Tick z => Int -> Outp -> Controller z (Outp)
waitGetLast 0 out = return out
waitGetLast steps out = do outs <- wait steps
                           return $ last outs


waitWithHo :: (Tick z) => Double -> Outp  -> Controller z (Outp)
waitWithHo tdiff outp = do
  let rad     = getRad outp
      horad2  = hohmannTime1R2 rad (tdiff/2)
  hohmannEllipse outp horad2


