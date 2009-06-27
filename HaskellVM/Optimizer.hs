module Optimizer where

import qualified Data.IntMap as I
import Data.List
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
getOptTime r
-}


test :: VM -> [(Addr,Dat)] -> ((Outp,Outp) -> Opt) -> ((Outp,Outp) -> Bool) -> (VM, [TOpt])
test vm cmds optfun abortcrit =
    let inps = zip [0..] $ (Inp $ I.fromList cmds) : (repeat $ Inp I.empty)                  
    in  helper inps [] (Outp I.empty) vm
        where helper :: [(Time,Inp)] -> [TOpt] -> Outp -> VM -> (VM, [TOpt]) 
              helper ((time, inp):rest) opts oldout vm = 
                  let (vm', out) = oneRun inp vm 
                      opt = optfun (oldout, out)
                      abb = abortcrit (oldout, out)
                  in case abb of 
                       True  -> (vm', opts)
                       False -> helper rest ((time,opt):opts) out vm'                     
    
testN :: VM -> [(Addr,Dat)] -> ((Outp,Outp) -> Opt) -> Time -> (VM, [TOpt])
testN vm cmds optfun maxtime =
    let inps = zip [0..] $ (Inp $ I.fromList cmds) : (repeat $ Inp I.empty) 
    in helper inps [] (Outp I.empty) vm
        where helper :: [(Time,Inp)] -> [TOpt] -> Outp -> VM -> (VM, [TOpt]) 
              helper ((time, inp):rest) opts oldout vm = 
                  let (vm', out) = oneRun inp vm 
                      opt = optfun (oldout, out)                      
                  in -- trace ((show time) ++ "\n" ++ (show out) ++ "\nOpt:" ++ (show opt)) $ 
                     case time >= maxtime of 
                       True  -> (vm', opts)
                       False -> helper rest ((time,opt):opts) out vm'
     

getOptTime :: [TOpt] -> (Time, Time, Opt)
getOptTime opts = 
    let (otime, ov ) = minimumBy (\ (_,o1) (_,o2) ->  compare o1 o2 ) $ opts
    in (fst $ head opts, otime, ov)

get :: Int -> Outp -> Dat
get k (Outp outp) = I.findWithDefault 0 k outp

getVel :: Int -> (Outp,Outp) -> Dat
getVel k (o1, o2) = get k o2 - get k o1

getRad :: Outp -> Dat
getRad o = sqrt $ (sqr $ get 2 o)  + (sqr $ get 3 o)

init1_1 :: [(Addr,Dat)]
init1_1 = [(16000,1001),(3, -2466.4860122222)]

isEmpty :: Outp -> Bool
isEmpty (Outp outp) = I.null outp 

opt1_1 :: (Outp,Outp) -> Opt
opt1_1 p@(old,o) = 
    let rad  = getRad o
        soll = get 4 o
        in abs (rad - soll)

crit1_1 :: (Outp,Outp) -> Bool
crit1_1 p@(old,o) 
    | isEmpty old = False
    | otherwise   = 
        let vx = getVel 2 p
        in vx < 0


test1_1 = do    
    vm <- loadVMFromFile "../task/bin1.obf"        
    let (vm',r) = test vm init1_1 opt1_1 crit1_1
    print $ getOptTime r
    