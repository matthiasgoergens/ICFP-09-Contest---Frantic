module Main where

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Util
import Types
import Load

import System
import Control.Monad


oneRun :: VM -> VM
oneRun vm = foldr (step) vm $ instr vm
 where step :: (Int, Instr) -> VM -> VM
       step (i, SType sop addr) vm = 
           let v1 = readMem vm addr
               in case sop of
                    Noop -> vm
                    Cmpz f -> vm {status = f v1 0}
                    Sqrt -> writeMem i (sqrt v1) vm
                    Copy -> writeMem i (v1) vm
                    Input -> writeMem i (readInput vm addr) vm
       step (i, DType dop addr1 addr2) vm = 
           let v1 = readMem vm addr1
               v2 = readMem vm addr2 
               in case dop of
                    Add -> writeMem i (v1 + v2) vm
                    Sub -> writeMem i (v1 - v2) vm
                    Mult-> writeMem i (v1 * v2) vm
                    Div -> writeMem i (if v2 == 0.0 then 0.0 else v1 / v2) vm
                    Output -> writeOutput (addr1) v2 vm
                    Phi -> writeMem i (if status vm then v1  else v2) vm
           

console :: VM -> IO()
console vm = helper vm
      where           
        helper :: VM -> IO()
        helper vm = do
             ls <- readConsoleLines
             (vm') <- loop ls vm
             case (isFinished vm') of
               True  -> putStrLn $"Finished with score " ++ (show $ score vm)                   
               False -> helper vm'
        loop :: [String] -> VM -> IO(VM)
        loop ls vm = do
               let inputdat  = map (readConsoleInput) ls
                   (out,vm') = shell inputdat vm
               putStr $ unlines $ map showConsoleOutput out 
               return (vm')
        
shell  :: [(Addr,Dat)] -> VM -> ([(Addr,Dat)],VM)
shell inputs vm = 
    let vm' = oneRun (setInputs inputs vm)
        outs = I.toAscList $ output vm'
        in (outs, vm')

main = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: vm binary confnum\n"; 
  let file = args !! 0
      conf = args !! 1
  dat <- B.readFile file
  let vm  = loadVM dat
      vm' = setInputs [(0x3E80, read conf)] vm 
  print vm'
  console vm'
--  let vm' = oneRun vm
--  print vm'
--  let vm'' = take 100 $ iterate oneRun vm
--  print vm''
  
  