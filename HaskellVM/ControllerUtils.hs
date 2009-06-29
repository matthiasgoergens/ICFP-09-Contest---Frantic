module ControllerUtils where

import Types
import Util
import qualified Data.IntMap as I

import Data.Function
import Test.QuickCheck

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


                                 

{-
[00:36:37] Alexander Kiel: public static double hohmannTime1R2(double r1, int th) {
        return pow(8 * mu, 1d / 3) * pow(th / PI, 2d / 3) - r1;
    }
[00:43:36] public static double circulationTime(double r) {
        return sqrt(4 * PI * PI / mu) * pow(r, 3d / 2);
    }

-}
