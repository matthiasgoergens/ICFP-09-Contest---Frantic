module ControllerUtils where

import Types
import Util
import qualified Data.IntMap as I




mu = let g = 6.67428E-11
         m_e = 6e24
     in g * m_e;

hohmannSpeed1 :: Dat -> Dat -> Dat
hohmannSpeed1 r1 r2 = 
    sqrt(mu / r1) * (sqrt (2 * r2 / (r1 + r2)) - 1)


hohmannSpeed2 :: Dat -> Dat -> Dat
hohmannSpeed2 r1 r2 = 
    sqrt(mu / r2) * (1 - sqrt(2 * r1 / (r1 + r2)))

hohmannTime :: Dat -> Dat -> Dat
hohmannTime r1 r2 = pi * sqrt ((r1 + r2)^3 / (8 * mu)) 


--- fliehkraft = Gravitation 
--   m_s v^2 /r = mu m_s / r^2
vOnCirc :: Dat -> Dat
vOnCirc r =  sqrt (mu/r)

-- time for one orbit = 2pi/omega = 2pi*r/v
timeOnCirc :: Dat -> Dat
timeOnCirc r =  2*pi*r / (vOnCirc r)


calcTick :: (Pos, Vec) -> (Pos,Vec)
calcTick (pos0,v0) = 
    let pos1 = pos0 + v0 + (scale 0.5 (calcG pos0))
    in (pos1, v0 + scale 0.5 ((calcG pos0) + (calcG pos1)) )


-- calcG pos = scale (mu / (vecLen2 pos)) $ normalize pos

calcG pos@(x,y) = scale (mu / ((vecLen2 pos))) $ (sqrt (1-y/x),sqrt (1-1/ratio))
    where ratio = y/x


{-
[00:36:37] Alexander Kiel: public static double hohmannTime1R2(double r1, int th) {
        return pow(8 * mu, 1d / 3) * pow(th / PI, 2d / 3) - r1;
    }
[00:43:36] public static double circulationTime(double r) {
        return sqrt(4 * PI * PI / mu) * pow(r, 3d / 2);
    }

-}
