module GradientDescent where
   

import Util
import Data.List
import Data.Function
import Debug.Trace

{-
instance Num a => Num [a] where
    (+) (as) (bs)   = map (uncurry (+)) $ zip as bs
    (-) (as) (bs)   = map (uncurry (-)) $ zip as bs
    (*) (as) (bs)   = map (uncurry (*)) $ zip as bs
    negate as       = map negate as
    abs as          = map abs as
    signum as       = map signum as
    fromInteger i   = trace ("Attention, fromInt on list " ++ show i)
                             $ repeat $ fromInteger i -- this sucks
-}

plus :: (Num a) => [a] -> [a] -> [a]
plus (as) (bs)   = map (uncurry (+)) $ zip as bs

mult :: (Num a) => [a] -> a -> [a]
mult l f = map ( * f ) l
divL :: (Num a, Fractional a) => [a] -> a  -> [a]
divL l f = map ( / f ) l
divR :: (Num a, Fractional a) => a  -> [a] -> [a]
divR f l = map ( f /  ) l


-------  Gradient Descent Algos 
--- Params
data OptParams a =  OptParams { eps :: a
                              , delta :: a
                              , maxiter :: Int
                              } deriving (Show)

defParams =  OptParams { eps =1
                       , delta = 0.0001
                       , maxiter = 50
                       } 

-- like takeWhile but keeps the element where the predicate fails
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f []     = []
takeWhile1 f (x:[]) = [x]
takeWhile1 f (x:xs) = if f x then x:takeWhile1 f xs else x:[head xs]


type EPS   = Double

-- | optimizes (minimizeses)
--   the function starting from initial point
--optimize :: (RealFrac p,Fractional p) => OptParams p -> (p -> Bool) -> ([p] -> p) -> [p] -> ([p],p)
optimize :: OptParams Double -> (Double -> Bool) -> ([Double] -> Double) -> [Double] -> ([Double],Double)
optimize conf abbort func initial =
    let val = func initial        
        steps    = iterate loop ( initial
                                , replicate dim (delta conf)
                                , val
                                , replicate dim (eps conf)
                                )                  
        optsteps = takeWhile1 (not . abbort . trd4) 
                   $ take (maxiter conf) 
                   $ steps        
        (par,_,opt,_) = minimumBy (compare `on` abs . trd4) 
                        $ trace (unlines $ map (show . trd4) optsteps) 
                        $ optsteps
    in (par,opt)
 where dim = length initial
--       loop :: (RealFrac p,Fractional p) => ([p], [p], p, p) -> ([p], [p], p, p)
       loop :: ([Double], [Double], Double, [EPS]) -> ([Double], [Double], Double, [EPS])
       loop (param, h, opt, es) = 
           let res    = map (helper opt param) $ zip3 [0..] h es
               param' = trace ("eps: " ++ show es) $ (param `plus` h)
               opt'   = func param'
               in (param', map fst res, opt', map snd res)               
       helper :: Double -> [Double] -> (Int,Double,EPS) -> (Double,EPS)
       helper opt p (i,d,e) = 
           let opt' =  func (combAtWith i d (+) p)
               grad = (opt'-opt) / d
           in if abs(opt') > abs(opt) then 
                  ( -d/2,  e/2) else 
                  ( (-e * opt) / grad, e)
--  Division by 0!

combAtWith :: Int -> a -> (a-> a -> a) -> [a] -> [a]
combAtWith 0   v f l = f (head l) v : (drop 1 l)
combAtWith pos v f l = let (a,b) = splitAt pos l
                           in a ++ [f (head b) v] ++ drop 1 b


-- | optimizes (minimizeses)
--   the function starting from initial point
optimize1 :: (Ord p, Num p, RealFrac p) => (OptParams p) -> (p -> Bool) -> (p -> p) -> p -> (p,p)
optimize1 conf abbort func initial =
    let val = func initial        
        steps    = iterate loop (initial, delta conf, val, (eps conf)) 
        optsteps = takeWhile1 (not . abbort . trd4) 
                   $ take (maxiter conf) 
                   $ steps        
        (par,_,opt,_) = minimumBy (compare `on` abs . trd4) 
--                      $ trace (unlines $ map (show . trd4) optsteps) 
                        $ optsteps
    in (par,opt)
 where loop (param, h, opt, eps) = 
           let param' = param + h
               opt'   = func param'                
               grad   = (opt'-opt) / h
           in if abs(opt')> abs(opt) then 
                  (param, -h/2, opt, eps/2) else
                  (param', -eps * opt/grad, opt', eps)

-- test: optimize1 defParams ( (< 0.00001) . abs) (\x -> x*x + 100) 5    


-- test2: optimize defParams ( (< 0.00001) . abs) (\x -> x*x + 100) 5    
testfun :: [Double] -> Double
testfun (x:y:_) = x*(y+10) + 10
testfun [] = error "empty list"
testfun k  = error $ "list with wrong length" ++ show k

testfun2 :: [Double] -> Double
testfun2 (x:y:_) = x*x*(y*y/10+10) + 10
testfun2 [] = error "empty list"
testfun2 k  = error $ "list with wrong length" ++ show k
