module ControllerUtils where

import Types
import qualified Data.IntMap as I


get :: Int -> Outp -> Dat
get k (Outp outp) = I.findWithDefault 0 k outp

getVel :: Int -> (Outp,Outp) -> Dat
getVel k (o1, o2) = get k o2 - get k o1

normalize :: (Dat,Dat) -> (Dat,Dat)
normalize v@(x,y) = let len = (1/vecLen v) in (x*len,y*len)

scalar :: (Dat,Dat) -> (Dat,Dat) -> Dat
scalar (a,b) (c,d) = a*c+b*d

-- (gegen den Urzeiger 90 grad )
perpendicular :: (Dat,Dat) -> (Dat,Dat) 
perpendicular (x,y) = (-y,x)

getRad :: Outp -> Dat
getRad o = vecLen ( get 2 o, get 3 o)


isEmpty :: Outp -> Bool
isEmpty (Outp outp) = I.null outp 

vecLen :: (Dat,Dat) -> Dat
vecLen (d1,d2) = sqrt(d1*d1 + d2*d2)


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
