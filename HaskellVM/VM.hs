module VM where

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
    in mapfst incTime $ foldl (flip step) (vm, out) $ instr vm 
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