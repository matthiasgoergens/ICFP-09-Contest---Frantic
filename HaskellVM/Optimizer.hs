
module Optimizer where

import qualified Data.IntMap as I
import Data.List
import Data.Function
import Debug.Trace

import Load
import VM
import Types
import Util

import ControllerUtils

type Time = Int
type Opt = Double
type TOpt = (Time,Double)

type CritFun = ((Outp,Outp) -> Time -> Bool)
type OptFun = ((Outp,Outp) -> Time -> Opt)
type FitFun = [TOpt] -> (Time, Opt)
    
---- Functions to test criterion and OptFun

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

---- Functions to test OptFun for N steps

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

--- Function to interate N steps

getN :: VM -> [(Addr,Dat)] -> Time -> VM
getN vm cmds  maxtime =
    let inps = take maxtime $ (Inp $ I.fromList cmds) : (repeat $ Inp I.empty) 
    in foldl (\v i -> fst $ oneRun i v  ) vm inps


-------  Gradient Descent Algos 
--- Params
data OptParams =  OptParams { eps :: Dat
                            , delta :: Dat
                            , thresh :: Opt 
                            , maxiter :: Int
                            } deriving (Show)

defParams =  OptParams { eps =0.1
                       , delta = 0.0001
                       , thresh = 10 
                       , maxiter = 10
                       } 

---  Gradient Descent for 1 Param

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f (x:xs) = x : takeWhile f xs


-- optimizes parameter at Addr to minimize FitFun
optimizer1 :: OptParams -> VM -> [(Addr,Dat)] -> Addr -> OptFun -> CritFun -> FitFun -> ([(Addr,Dat)],Dat,Opt,Time)
optimizer1 params vm cmds port optfun abortcrit fitfun =
    let (_,r) = test vm cmds optfun abortcrit
        (t,opt) = fitfun r
        steps   = iterate loop (cmds, delta params, opt, t)                  
        optsteps = takeWhile1 ( (> (thresh params)) . trd4 ) 
                   $ take (maxiter params) 
                   $ steps        
    in minimumBy (compare `on` trd4) $ trace (unlines $ map (show . trd4) optsteps) $ optsteps
 where loop :: ([(Addr,Dat)], Dat, Opt, Time) -> ([(Addr,Dat)], Dat, Opt, Time)
       loop (cmds,h,opt, t) = 
           let cmdchange = mapsnd (\x -> h) $ head $ filter ( (== port) . fst ) cmds
               cmds'     = I.toList $ I.fromListWith (+) (cmdchange : cmds )        
               (delta',opt',t') = helper cmds' h opt
               in (cmds',delta',opt',t')
       helper cmds h oldopt = 
           let (_,r') = test vm cmds optfun abortcrit
               (t',opt') = fitfun r'
               grad     = (opt'-oldopt) / h
           in  (-(eps params)* oldopt/grad, opt',t')
          

changeParams :: (Addr, Dat) -> [(Addr,Dat)] -> [(Addr,Dat)]
changeParams (port,h) cmds  = 
    let cmdchange1 = mapsnd (\x -> h) $ head $ filter ( (== port) . fst ) cmds
        in I.toList $ I.fromListWith (+) (cmdchange1 : cmds ) 

-- optimizes parameters at Addr1 and Addr2 to minimize FitFun
optimizer2 :: OptParams -> VM -> [(Addr,Dat)] -> (Addr,Addr) -> OptFun -> CritFun -> FitFun -> ([(Addr,Dat)],(Dat,Dat),Opt,Time)
optimizer2 params vm cmds (port1,port2) optfun abortcrit fitfun =
    let (_,r) = test vm cmds optfun abortcrit
        (t,opt) = fitfun r
        steps   = iterate loop (cmds, (delta params, delta params), opt, t)
        optsteps = (cmds,(0,0),opt,t) : (takeWhile ( (> (thresh params)) . trd4 ) 
                   $ take (maxiter params) 
                   $ steps )
    in minimumBy (compare `on` trd4) $ trace (unlines $ map (show . trd4) optsteps) $ optsteps
 where loop :: ([(Addr,Dat)], (Dat,Dat), Opt, Time) -> ([(Addr,Dat)], (Dat,Dat), Opt, Time)
       loop (cmds,(h1,h2),opt, t) = 
           let cmds1      = changeParams (port1,h1) cmds 
               (delta1,opt1,t1) = helper cmds1 h1 opt
               cmds2      = changeParams (port2,h2) cmds 
               (delta2,opt2,t2) = helper cmds2 h2 opt
               cmds3      = changeParams (port1,h1) $ changeParams (port2,h2) cmds
               in (cmds3,(delta1,delta2),min opt1 opt2, min t1 t2)
       helper cmds h oldopt = 
           let (_,r') = test vm cmds optfun abortcrit
               (t',opt') = fitfun r'
               grad     = (opt'-oldopt) / h
           in  (-(eps params)* oldopt/grad, opt',t')

    
getRunTime :: [TOpt] -> Time
getRunTime opts = fst $ head opts

getOptTimeMin :: [TOpt] -> (Time, Opt)
getOptTimeMin opts = minimumBy (\ (_,o1) (_,o2) ->  compare o1 o2 ) $ opts


getOptTimeMax :: [TOpt] -> (Time, Opt)
getOptTimeMax opts =  maximumBy (\ (_,o1) (_,o2) ->  compare o1 o2 ) $ opts

---
   

printStat :: ([(Addr,Dat)],Dat,Opt,Time) -> IO()
printStat (newcmds, _ , opt, time) = 
    putStrLn $ "Opts and Time: " ++ (show time) 
                 ++ "\nNewCommands: " ++ (show newcmds) 
                 ++ "\nOpt: " ++ (show opt)

showStat2 :: ([(Addr,Dat)],(Dat,Dat),Opt,Time) -> String
showStat2 (newcmds, _ , opt, time) = 
    "Opts and Time: " ++ (show time) 
                          ++ "\nNewCommands: " ++ (show newcmds) 
                          ++ "\nOpt: " ++ (show opt)

---------
-- PROBLEM 1

{-

-- 1001
vm <- loadVMFromFile "../task/bin1.obf"    
let params = defParams { thresh = 150 }
let stat@(newcmds, _ , opt, time) = optimizer1 params vm init1_1a 3 opt1_1a crit1_1a getOptTimeMin
printStat stat

let vm2 = getN vm newcmds time
let stat2 = optimizer1 params vm2 init1_1b 3 optOrbit crit1_1b getOptTimeMax
printStat stat2
-- [(3,-2466.484135158279),(16000,1001.0)]
-- 18868 : [(3,1473.167973436527)]

-- 1002
let initcmd = (16000,1002) :: (Addr,Dat)
vm <- loadVMFromFile "../task/bin1.obf"    
let (vm1,out1) = oneRun (mkInp [initcmd]) vm 
doHohmann vm1 out1 (get 4 out1) 1




let initcmd = (16000,1002) :: (Addr,Dat)
vm <- loadVMFromFile "../task/bin1.obf"    
let (_,out1) = oneRun (mkInp [initcmd]) vm 
out1

let rad1 = getRad out1
let rad2 = get 4 out1
let transtime = fromIntegral $ ceiling $ hohmannTime rad1 rad2 :: Time
let force1 = hohmannSpeed1 rad1 rad2
let force2 = hohmannSpeed1 rad1 rad2
let (vx,vy) = normalize(- get 3 out1, get 2 out1) -- perpendicular to earth
let init1  = initcmd : [(2,force1 * vx), (3,force1 * vy)] 
transtime
init1
let params = defParams { thresh = 100, eps=0.5 }
let (stat@(newcmds, _ , opt, time)) = optimizer2 params vm init1 (2,3) optOrbit (critTime (transtime+20))  getOptTimeMin
printStat2 stat

let vm2 = getN vm newcmds time
let (_,out2) = oneRun (mkInp [initcmd]) vm2 
let (vx2,vy2) = normalize(- get 3 out2, get 2 out2) -- perpendicular to earth
let init2  = [(2,force2 * vx2), (3,force2 * vy2)] :: [(Addr,Dat)]
let (stat2@(newcmds2, _ , opt2, time2)) = optimizer2 params vm2 init2 (2,3) optOrbit (critTime 5000)  getOptTimeMax
printStat2 stat2

----  Test
let initcmd = (16000,1002) :: (Addr,Dat)
vm <- loadVMFromFile "../task/bin1.obf"    
let (vm1,out1) = oneRun (mkInp [initcmd]) vm 
out1
let rad1 = getRad out1
let rad2 = get 4 out1
let transtime = fromIntegral $ ceiling $ hohmannTime rad1 rad2 :: Time
let force1 = hohmannSpeed1 rad1 rad2
let force2 = hohmannSpeed1 rad1 rad2
let pos    = (get 2 out1, get 3 out1)
let (vx,vy) = normalize $ perpendicular $ pos -- perpendicular to earth
let goalPoint = (normalize $ -pos) * (rad2,rad2)
let init1  = initcmd : [(2,force1 * vx), (3,force1 * vy)] 
transtime
init1

let params = defParams { thresh = 10, eps=0.1 }
let (stat@(newcmds, _ , opt, time)) = optimizer2 params vm init1 (2,3) (optPoint goalPoint) (critTime (transtime+20))  getOptTimeMin
putStr $ showStat2 stat



let vm2 = getN vm1 init1 (transtime-1)
let (vm3,out2) = oneRun (mkInp []) vm2 
let (vx2,vy2) = normalize(- get 3 out2, get 2 out2) -- perpendicular to earth
let rad2 = getRad out2


let init2  = [(2,force2 * vx2), (3,force2 * vy2)] :: [(Addr,Dat)]
init2
let params = defParams { thresh = 100, maxiter = 50,  eps=0.01}
let (stat2@(newcmds2, _ , opt2, time2)) = optimizer2 params vm3 init2 (2,3) (optMyOrbit rad2) (critTime 500) getOptTimeMax
printStat2 stat2



-}

type InpSeq = [(Time,Inp)]

doHohmann :: (VM, Outp) -> Dat -> Time -> ((VM, Outp), InpSeq)
doHohmann (vm, out1) sollrad time = 
    let rad1 = getRad out1
        transtime = fromIntegral $ ceiling $ hohmannTime rad1 sollrad :: Time
        force1 = hohmannSpeed1 rad1 sollrad
        pos    = (get 2 out1, get 3 out1)
        (vx,vy) = normalize $ perpendicular $ pos -- perpendicular to earth
        goalPoint = (normalize $ -pos) * (sollrad,sollrad)
        cmds   = [(2,force1 * vx), (3,force1 * vy)] 
        params = defParams { thresh = 10, eps=0.1 }
        stat = optimizer2 params vm cmds (2,3) (optPoint goalPoint) (critTime (transtime+20)) getOptTimeMin
        (newcmds, _ , _, opttime) = stat

        vm2 = getN vm newcmds (opttime-1) -- we go one step before to check for position
        (vm3,out2) = oneRun (mkInp []) vm2 -- this is the step where we apply the second force
        (vx2,vy2) = normalize(- get 3 out2, get 2 out2) -- new force 
        rad2  = getRad out2
        force2 = hohmannSpeed2 rad1 rad2 -- use true radius
        -- optimize second force
        cmds2   = [(2,force2 * vx2), (3,force2 * vy2)] :: [(Addr,Dat)]
        params2 = defParams { thresh = 10, eps=0.1 }
        stat2 = optimizer2 params2 vm3 cmds2 (2,3) (optMyOrbit rad2) (critTime 1000) getOptTimeMax
       
    in trace ((showStat2 stat) ++ "\n" ++ (showStat2 stat2)) $ 
           ((vm3,out2), [(time, mkInp newcmds) , (time + opttime, mkInp cmds2) ])
       


critTime :: Time -> (Outp,Outp) -> Time -> Bool
critTime maxtime _ t = t >= maxtime 

-- optimal orbit
optPoint point p@(old,o) _ =
    let x = get 2 o
        y = get 3 o
        in vecLen ((x,y) - point)

-- optimal orbit
optMyOrbit sollrad p@(old,o) _ =     let rad  = getRad o in abs (rad - sollrad)


init1_1a,init1_1b :: [(Addr,Dat)]
-- init1_1 = [(16000,1001),(3, -2466.4860122222)]
init1_1a= [(16000,1001),(3, -2466.4860122222 - 1.3176340142438757e-5)]

init1_1b = [(3, 1482.9355671460403)]

-- optimal point
opt1_1a :: (Outp,Outp) -> Time -> Opt
opt1_1a p@(old,o) _ = 
    let sollr = get 4 o
        x = get 2 o
        y = get 3 o
        in vecLen ((x,y) - (sollr,0))

-- optimal orbit
optOrbit p@(old,o) _ = 
    let rad  = getRad o
        soll = get 4 o
        in abs (rad - soll)

crit1_1a :: (Outp,Outp) -> Time -> Bool
crit1_1a p@(old,o) _ 
    | isEmpty old = False
    | otherwise   = 
        let vx = getVel 2 p
        in vx < 0

crit1_1b _ t = t > 900


test1_1 = do    
    vm <- loadVMFromFile "../task/bin1.obf"        
    let (vm',r) = test vm init1_1a opt1_1a crit1_1a
    let (opttime, optval) = getOptTimeMin r
    let totaltime         = getRunTime r
    print $ totaltime
    print $ getOptTimeMin r
    let vm2 = getN vm init1_1a opttime
    let (vm',r) = test vm2 init1_1b opt1_1a crit1_1b
    print $ getOptTimeMax r


--- 1_2
init1_2a :: [(Addr,Dat)]
init1_2a = [(3,-2499.8427383354174),(16000,1002.0)]

crit1_2a :: (Outp,Outp) -> Time -> Bool
crit1_2a p@(old,o) _ 
    | isEmpty old = False
    | otherwise   = 
        let v = (getVel 2 p, getVel 3 p)
            toEarth = normalize( get 2 o, get 3 o) -- to earth
            vToEath = scalar v toEarth
        in trace (show toEarth) $ vToEath < 0



mkInputFile :: Int -> Dat -> InpSeq -> String
mkInputFile numEmpties conf inps = 
    let all = (0,mkInp [(16000, conf)]) : inps
        noCons = map ( mapsnd (\ (Inp m) -> m)) all
        imap = I.fromListWith (I.union) noCons
        max  = numEmpties + (maximum $ I.keys imap)
        allsteps = map (\t -> I.findWithDefault I.empty t imap) $ take max [0..]
    in unlines $ map showStep allsteps
       where showStep :: (I.IntMap Dat) -> String
             showStep inp = concat $ filter (not . null) $  (map (\(a,d) -> show a ++ " " ++ show d ++ "\n") $ I.toList inp) ++ ["."]

solveTask1 :: String -> Dat -> IO()
solveTask1 file conf = do
  let initcmd = (16000,conf) :: (Addr,Dat)
  vm <- loadVMFromFile "../task/bin1.obf"    
  let (vm1,out1) = oneRun (mkInp [initcmd]) vm 
      ((vm',out'),inpseq) = doHohmann (vm1,out1) (get 4 out1) 1
  print inpseq
  writeFile file $ mkInputFile 2000 conf inpseq



----
{- 
 Task 2

let inpseq = [(1,Inp (I.fromList [(2,-869.4326035230357),(3,-868.1635465846844)])),(9152,Inp (I.fromList [(2,699.1992759101576),(3,698.4081077018907)]))] :: InpSeq
putStr $ mkInputFile 2000 1002 inpseq

Task 3
[(1,Inp (fromList [(2,-1.3715393687227442),(3,-1672.6902870476797)])),(12221,Inp (fromList [(2,0.8948715955925662),(3,1219.114292914203)]))]

Task 4
[(1,Inp (fromList [(2,5446.082488748258),(3,-1693416.539137936)])),(27,Inp (fromList [(2,-1496.3360879621182),(3,2.947883974288544)]))]


-}