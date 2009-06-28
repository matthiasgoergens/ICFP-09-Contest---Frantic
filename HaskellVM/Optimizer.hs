{-# LANGUAGE FlexibleInstances #-}

module Optimizer where

import qualified Data.IntMap as I
import Data.List
import Data.Function
import Debug.Trace

import Load
import VM
import Types
import Util

type Time = Int
type Opt = Double
type TOpt = (Time,Double)

{-

vm <- loadVMFromFile "../task/bin1.obf"    
let (vm',r) = testN vm init1_1 opt1_1 10    
let (vm',r) = test vm init1_1 opt1_1 crit1_1
getOptTimeMin r
optimizer2 vm init1_1 1 0.001 opt1_1 crit1_1 getOptTimeMin

vm <- loadVMFromFile "../task/bin1.obf"    
optimizer1 defParams vm init1_1 3 opt1_1a crit1_1 getOptTimeMin


-}

type CritFun = ((Outp,Outp) -> Time -> Bool)
type OptFun = ((Outp,Outp) -> Time -> Opt)
type FitFun = [TOpt] -> (Time, Opt)
    

test :: VM -> [(Addr,Dat)] -> OptFun -> CritFun -> (VM, [TOpt])
test vm cmds optfun abortcrit =
    let inps = zip [0..] $ (Inp $ I.fromList cmds) : (repeat $ Inp I.empty)                  
    in  helper inps [] (Outp I.empty) vm
        where helper :: [(Time,Inp)] -> [TOpt] -> Outp -> VM -> (VM, [TOpt]) 
              helper ((time, inp):rest) opts oldout vm = 
                  let (vm', out) = oneRun inp vm 
                      opt = optfun (oldout, out) time 
                      abb = abortcrit (oldout, out) time
                  in case abb of 
                       True  -> (vm', opts)
                       False -> helper rest ((time,opt):opts) out vm'                     

-- fixed number of steps
testN :: VM -> [(Addr,Dat)] -> OptFun -> Time -> (VM, [TOpt])
testN vm cmds optfun maxtime =
    let inps = zip [0..] $ (Inp $ I.fromList cmds) : (repeat $ Inp I.empty) 
    in helper inps [] (Outp I.empty) vm
        where helper :: [(Time,Inp)] -> [TOpt] -> Outp -> VM -> (VM, [TOpt]) 
              helper ((time, inp):rest) opts oldout vm = 
                  let (vm', out) = oneRun inp vm 
                      opt = optfun (oldout, out) time
                  in trace ((show time) ++ "\n" ++ (show out) ++ "\nOpt:" ++ (show opt)) $ 
                     case time >= maxtime of 
                       True  -> (vm', opts)
                       False -> helper rest ((time,opt):opts) out vm'

optimizer :: VM -> [(Addr,Dat)] -> Int -> OptFun -> CritFun -> FitFun -> (Opt,[(Addr,Dat)])
optimizer vm cmds numparams optfun abortcrit fitfun=
    let (_,r) = test vm cmds optfun abortcrit
        (t,opt) = fitfun r
        cmdchange = map (mapsnd (\x -> 0.01)) $ (take 1 $ reverse cmds)
        cmds'   = I.toList $ I.fromListWith (+) (cmds ++ cmdchange)
        (_,r') = test vm cmds' optfun abortcrit
        (t',opt') = fitfun r'
        grad     = trace ((show opt)  ++ " " ++ (show opt')) $(opt'-opt) / 0.01
    in (grad,[])


data OptParams =  OptParams { eps :: Dat
                            , delta :: Dat
                            , thresh :: Opt 
                            , maxiter :: Int
                            } deriving (Show)

defParams =  OptParams { eps =0.3
                       , delta = 0.0001
                       , thresh = 10 
                       , maxiter = 10
                       } 

optimizer1 :: OptParams -> VM -> [(Addr,Dat)] -> Addr -> OptFun -> CritFun -> FitFun -> ([(Addr,Dat)],Dat,Opt)
optimizer1 params vm cmds port optfun abortcrit fitfun =
    let (_,r) = test vm cmds optfun abortcrit
        (t,opt) = fitfun r
        steps   = iterate loop (cmds, delta params, opt)
        optsteps = takeWhile ( (> (thresh params)) . trd ) $ take (maxiter params) $ trace (show $ take 10 steps) steps        
    in minimumBy (compare `on` trd) optsteps
 where loop :: ([(Addr,Dat)], Dat, Opt) -> ([(Addr,Dat)], Dat, Opt)
       loop (cmds,h,opt) = 
           let cmdchange = mapsnd (\x -> h) $ head $ filter ( (== port) . fst ) cmds
               cmds'     = I.toList $ I.fromListWith (+) (cmdchange : cmds )        
               (delta',opt') = helper cmds' h opt
               in (cmds',delta',opt')
       helper cmds h oldopt = 
           let (_,r') = test vm cmds optfun abortcrit
               (_,opt') = fitfun r'
               grad     = (opt'-oldopt) / h
           in  (-(eps params)* oldopt/grad, opt')
          


getN :: VM -> [(Addr,Dat)] -> Time -> VM
getN vm cmds  maxtime =
    let inps = take maxtime $ (Inp $ I.fromList cmds) : (repeat $ Inp I.empty) 
    in foldl (\v i -> fst $ oneRun i v  ) vm inps
    
getRunTime :: [TOpt] -> Time
getRunTime opts = fst $ head opts

getOptTimeMin :: [TOpt] -> (Time, Opt)
getOptTimeMin opts = minimumBy (\ (_,o1) (_,o2) ->  compare o1 o2 ) $ opts


getOptTimeMax :: [TOpt] -> (Time, Opt)
getOptTimeMax opts =  maximumBy (\ (_,o1) (_,o2) ->  compare o1 o2 ) $ opts

---

instance Num a => Num (a,a) where
    (+) (a,c) (b,d) = (a+b,c+d)
    (-) (a,c) (b,d) = (a-b,c-d)
    (*) (a,c) (b,d) = (a*b,c*d)
    negate (a,b)    = (-a,-b)
    abs (a,b)       = (abs(a),abs(b))
    signum (a,b)    = (signum a, signum b) 
    fromInteger i   = (fromInteger i, fromInteger i) 
                      

trd :: (a,b,c) -> c
trd (_,_,c) = c
   
get :: Int -> Outp -> Dat
get k (Outp outp) = I.findWithDefault 0 k outp

getVel :: Int -> (Outp,Outp) -> Dat
getVel k (o1, o2) = get k o2 - get k o1

getRad :: Outp -> Dat
getRad o = sqrt $ (sqr $ get 2 o)  + (sqr $ get 3 o)

init1_1 :: [(Addr,Dat)]
-- init1_1 = [(16000,1001),(3, -2466.4860122222)]
init1_1 = [(16000,1001),(3, -2466.4860122222 - 1.3176340142438757e-5)]

init1_1b :: [(Addr,Dat)]
init1_1b = [(3, 1482.9355671460403)]


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


---------

opt1_1 :: (Outp,Outp) -> Time -> Opt
opt1_1 p@(old,o) _ = 
    let rad  = getRad o
        soll = get 4 o
        in abs (rad - soll)

-- optimal point
opt1_1a :: (Outp,Outp) -> Time -> Opt
opt1_1a p@(old,o) _ = 
    let sollr = get 4 o
        x = get 2 o
        y = get 3 o
        in vecLen ((x,y) - (sollr,0))


crit1_1 :: (Outp,Outp) -> Time -> Bool
crit1_1 p@(old,o) _ 
    | isEmpty old = False
    | otherwise   = 
        let vx = getVel 2 p
        in vx < 0

crit1_1b :: (Outp,Outp) -> Time -> Bool
crit1_1b _ t = t > 900


test1_1 = do    
    vm <- loadVMFromFile "../task/bin1.obf"        
    let (vm',r) = test vm init1_1 opt1_1a crit1_1
    let (opttime, optval) = getOptTimeMin r
    let totaltime         = getRunTime r
    print $ totaltime
    print $ getOptTimeMin r
    let vm2 = getN vm init1_1 opttime
    let (vm',r) = test vm2 init1_1b opt1_1 crit1_1b
    print $ getOptTimeMax r



    

