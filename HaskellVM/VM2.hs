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

import Data.Array.IArray as IA
import Data.Array
import Control.Monad.Writer

import Console

type Mem = Array Int Dat
type Status = Bool
type InPorts = I.IntMap Dat
type OutPorts = I.IntMap Dat

map2array :: I.IntMap Dat -> Mem
map2array iMap = IA.array (0,2^16-1) . I.toList $ iMap

array2map :: Mem -> I.IntMap Dat
array2map = I.fromList . IA.assocs


-- manyRuns koennte so aussehen:
-- manyRuns :: Int -> [(Int, Instr)] -> Status -> Mem -> [InPorts] -> [(Status, Mem, OutPorts)]
-- manyRuns size code startStatus startMem inputs = ...


oneRun :: Inp -> VM -> (VM, Outp)
oneRun (Inp inp) vm = (vm { mem = array2map newMem
                          ,  status = newStatus
                          ,  time = time vm + 1
                          }
                , Outp outPorts)
    where (newStatus, newMem, outPorts) = oneRun' (size vm) (instr vm) (status vm) (map2array (mem vm)) inp
    

oneRun' :: Int -> [(Int, Instr)] -> Status -> Mem -> InPorts -> (Status, Mem, OutPorts)
oneRun' size code oldStatus oldMem input = (newStatus, newMem, outPorts)
    where helper :: Int -> Instr -> Status -> (Status, Dat, OutPorts -> OutPorts)
          helper c (SType sop addr) z
              = trace (show c) $
                let v1 = readMem c addr
                    vc = (IA.!) oldMem c
                    input = readInput addr
                in case sop of 
                     Noop -> (z, vc, id)
                     Cmpz f -> (f v1 0, vc, id)
                     Sqrt -> (z, sqrt v1, id)
                     Copy -> (z, vc, id)
                     Input -> (z, input, id)
          helper c (DType dop addr1 addr2) z
              = trace (show c) $
                let v1 = readMem c addr1
                    v2 = readMem c addr2
                    vc = (IA.!) oldMem c
                in case dop of
                    Add -> (z, (v1 + v2), id)
                    Sub -> (z, v1 - v2, id)
                    Mult-> (z, v1 * v2, id)
                    Div -> (z, (if v2 == 0.0 then 0.0 else v1 / v2), id)
                    Output -> (z, vc, writeOutput addr1 v2)
                    Phi -> (z, if z then v1 else v2, id)

          writeOutput = I.insert
          
          op :: (Status -> (Status, Dat, OutPorts -> OutPorts))
             -> (Status, (OutPorts -> OutPorts))
             -> ((Status, (OutPorts -> OutPorts)) -> EndList Dat (Status, OutPorts -> OutPorts))
             -> EndList Dat (Status, OutPorts -> OutPorts)
          op f (oldStatus, oldOut) cont
              = Cons dat (cont (newStatus, (newOut . oldOut)))
              where (newStatus, dat, newOut) = f oldStatus
          endList = foldEndList Nil (oldStatus, id)
                    . L.map op . L.map (uncurry helper) $ code

          (newStatus, outPortsTrans) = end endList

          outPorts = outPortsTrans I.empty
          newMemPre = toList endList
          newMem = IA.array (0, size - 1) (zip [0..] newMemPre)

          readMem c i | i <= c = (IA.!) oldMem i
                      | otherwise = (IA.!) newMem i

          readInput :: Int -> Dat
          readInput = flip (I.findWithDefault 0) input
          
-- (Status -> (Status, Dat, Outports -> Outports))

data EndList a b = Cons a (EndList a b) | Nil b deriving (Ord, Eq, Show)

end (Nil b) = b
end (Cons _ b) = end b
toList (Cons a b) = a : toList b
toList (Nil _) = []

foldEndList ::  (z -> c) -> z -> [(z -> (z -> c) -> c)] -> c
foldEndList opNil z [] = opNil z
foldEndList opNil z (opCons : xs) = opCons z cont
    where cont newZ = foldEndList opNil newZ xs


-- oneRun :: Inp -> VM -> (VM, Outp)
-- oneRun inp vm = 
--     let out = Outp I.empty
--     in mapfst incTime $ foldl (flip step) (vm, out) $ instr vm 
--  where step :: (Int, Instr) -> (VM,Outp) -> (VM,Outp)
--        step (i, SType sop addr) (vm,out) =
--            let v1 = readMem vm addr
--                in case sop of
--                     Noop -> (vm,out)
--                     Cmpz f -> (vm {status = f v1 0},out)
--                     Sqrt -> (writeMem i (sqrt v1) vm,out)
--                     Copy -> (writeMem i (v1) vm,out)
--                     Input -> (writeMem i (readInput inp addr) vm,out)
--        step (i, DType dop addr1 addr2) (vm,out) = 
--            let v1 = readMem vm addr1
--                v2 = readMem vm addr2 
--                in case dop of
--                     Add -> (writeMem i (v1 + v2) vm,out)
--                     Sub -> (writeMem i (v1 - v2) vm,out)
--                     Mult-> (writeMem i (v1 * v2) vm,out)
--                     Div -> (writeMem i (if v2 == 0.0 then 0.0 else v1 / v2) vm,out)
--                     Output -> (vm, writeOutput (addr1) v2 out)
--                     Phi -> (writeMem i (if status vm then v1  else v2) vm,out)
           


        
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
  console oneRun vm
--  let vm' = oneRun vm
--  print vm'
--  let vm'' = take 100 $ iterate oneRun vm
--  print vm''

 -- 16000 1001
  
  