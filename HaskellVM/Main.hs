module Main where

import qualified Data.ByteString as B
import qualified Data.IntMap as IntMap

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
                    Cmpz f -> vm {status = f 0 v1} -- TODO check order!
                    Sqrt -> writeMem i (sqrt v1) vm
                    Copy -> writeMem i (v1) vm
                    Input -> writeMem i (readInput vm (floor v1)) vm
       step (i, DType dop addr1 addr2) vm = 
           let v1 = readMem vm addr1
               v2 = readMem vm addr2 
               in case dop of
                    Add -> writeMem i (v1 + v2) vm
                    Sub -> writeMem i (v1 - v2) vm
                    Mult-> writeMem i (v1 * v2) vm
                    Div -> writeMem i (v1 / v2) vm
                    Output -> writeOutput (floor v1) v2 vm
                    Phi -> writeMem i (if status vm then v1  else v2) vm
           

main = do
  args <- getArgs
  let file = args !! 0
  dat <- B.readFile file
  let vm  = loadVM dat                  
  print vm
  let vm' = oneRun vm
  print vm'
  let vm'' = take 100 $ iterate oneRun vm
  print vm''
  
  