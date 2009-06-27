module Main where

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Debug.Trace
import Util
import Types
import Load

import System
import Control.Monad


oneRun :: Inp -> VM -> (VM, Outp)
oneRun inp vm = 
    let out = Outp I.empty
    in foldl (flip step) (vm, out) $ instr vm
 where step :: (Int, Instr) -> (VM,Outp) -> (VM,Outp)
       step (i, SType sop addr) (vm,out) =
           let v1 = readMem vm addr
               in case sop of
                    Noop -> (vm,out)
                    Cmpz f -> (vm {status = f v1 0},out)
                    Sqrt -> (writeMem i (sqrt v1) vm,out)
                    Copy -> (writeMem i (v1) vm,out)
                    Input -> (writeMem i (readInput inp addr) vm,out)
       step (i, DType dop addr1 addr2) (vm,out) = 
           let v1 = readMem vm addr1
               v2 = readMem vm addr2 
               in case dop of
                    Add -> (writeMem i (v1 + v2) vm,out)
                    Sub -> (writeMem i (v1 - v2) vm,out)
                    Mult-> (writeMem i (v1 * v2) vm,out)
                    Div -> (writeMem i (if v2 == 0.0 then 0.0 else v1 / v2) vm,out)
                    Output -> (vm, writeOutput (addr1) v2 out)
                    Phi -> (writeMem i (if status vm then v1  else v2) vm,out)
           

console :: VM -> IO()
console vm = helper vm
      where           
        helper :: VM  -> IO()
        helper vm = do          
             ls <- readConsoleLines
             let inputdat   = map (readConsoleInput) ls
                 inp        = setInputs inputdat (Inp I.empty)
             (vm', out) <- loop inp vm
             case (isFinished out) of
               True  -> putStrLn $"Finished with score " ++ (show $ score out) 
               False -> helper vm'
        loop :: Inp -> VM -> IO(VM,Outp)
        loop inp vm = do          
          let (vm', out) = oneRun inp vm 
              Outp outmap = out
              Inp inmap   = inp
          putStr "#inp:"  
          putStrLn $ concat $ L.intersperse "#" $ map showConsoleOutput $ I.toAscList inmap
          putStrLn "#out:"  
          putStr $ unlines $ map showConsoleOutput $ I.toAscList outmap
          putStrLn "."
          return (vm', out)
        
{- shell  :: [(Addr,Dat)] -> (VM) -> (VM, [(Addr,Dat)])
shell inputs vm = 
    let inp = setInputs inputs (Input I.empty)
        (vm', out) = oneRun inp vm
        Output outmap = out
        outs = I.toAscList $ outmap
    in (vm', outs)
-}

main = do
  args <- getArgs
  when (length args < 1) $ do fail "\nUsage: vm binary\n\t first line \"16000 confignummer\"\n\tproceed with . \\n \n"; 
  let file = args !! 0
  dat <- B.readFile file
  let vm  = loadVM dat
-- print vm
  console vm
--  let vm' = oneRun vm
--  print vm'
--  let vm'' = take 100 $ iterate oneRun vm
--  print vm''

 -- 16000 1001
  
  