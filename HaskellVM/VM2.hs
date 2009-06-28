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

import Control.Monad.State.Strict

import Console

import Data.Function
import System.IO

import Data.Maybe

type Mem = Array Int Dat
type Status = Bool
type InPorts = I.IntMap Dat
type OutPorts = I.IntMap Dat

map2array :: I.IntMap Dat -> Mem
map2array iMap = IA.array (0,2^16-1) . I.toList $ iMap

array2map :: Mem -> I.IntMap Dat
array2map = I.fromList . IA.assocs


-- manyRuns koennte so aussehen:

manyRuns :: Int -> [(Int, Instr)] -> Status -> Mem -> [InPorts] -> [OutPorts]
manyRuns size code status mem inputs = evalState  (sequence $ map oneStep inputs) (status, mem)
    where oneStep input = oneRun'' size code input

-- oneRun'' :: Int -> [(Int, Instr)] -> InPorts -> State (Status, Mem) OutPorts


parseFullConsoleInput :: String -> [[(Addr, Dat)]]
parseFullConsoleInput = map (map readConsoleInput) . frames . cleanLines


cleanLines :: String -> [String]
cleanLines = filter (not . all (==' '))
             . filter (not . null)
             . filter (not . L.isPrefixOf "#")
             . lines

frames :: [String] -> [[String]]
frames [] =  []
frames s =  let (l, s') = break (L.isPrefixOf ".") s
            in  l : case s' of
                      []      -> []
                      (_:s'') -> frames s''
                        

console' :: VM -> IO ()
console' vm = do contents <- getContents
                 let inports :: [I.IntMap Dat]
                     inports = map I.fromList . parseFullConsoleInput $ contents
                     outports = manyRuns (size vm) (L.sortBy (compare `on` fst) $ instr vm) (status vm) (map2array (mem vm))
                                $ inports
                 sequence_ . map showFrame $ zip3 inports outports [1..]
    where showFrame (inmap, outmap, timeStep) = do
            putStrLn $ "#time: " ++ (show timeStep)
            putStr "#inp:"
            putStrLn $ concat $ L.intersperse "#" $ map showConsoleOutput $ I.toAscList inmap
            putStrLn "#out:"
            putStr $ unlines $ map showConsoleOutput $ I.toAscList outmap
            putStrLn "."
            hFlush stdout

-- prop_oneRun' = trace (show $ IA.elems mem) $
--                and [(newStatus == False)
--                    , I.null outports
--                    , IA.elems mem == [5,8,13]]
--     where (newStatus, mem, outports) =
--               (oneRun' 3 [(0,SType Noop 0)
--                          ,(1,SType Noop 0)
--                          ,(2,DType Add 0 1)
--                          ])
--                False
--                (IA.array (0,2) [(0,5)
--                                ,(1,8)
--                                ,(2,0)])
--                (I.fromList [(0,1)
--                            ,(1,2)
--                            ,(2,3)
--                            ]
--                )
               
oneRun'' :: Int -> [(Int, Instr)] -> InPorts -> State (Status, Mem) OutPorts
oneRun'' size code input = do (oldStatus, oldMem) <- get
                              let (newStatus, newMem, out) = oneRun' size code oldStatus oldMem input
                              put (newStatus, newMem)
                              return out
                              

oneRun' :: Int -> [(Int, Instr)] -> Status -> Mem -> InPorts -> (Status, Mem, OutPorts)
oneRun' size code oldStatus oldMem input = do (newStatus, newMem, outPorts)
    where       
          stateMonOneStep :: State Status [(Dat, Maybe (Addr, Dat))]
          stateMonOneStep = (sequence $ L.zipWith (runInstr (readInput' input))
                                          (L.map (readMem' oldMem newMem) . map fst $ code) code)
          (mem_trans, newStatus) = runState stateMonOneStep oldStatus
          (newMemPre, outPortsAssoc) = unzip mem_trans

          outPorts = foldr (uncurry I.insert) I.empty . reverse . catMaybes $ outPortsAssoc

          newMem = IA.array (0, size - 1) (zip [0..] newMemPre)

readInput' :: I.IntMap Dat -> Addr -> Dat
readInput' input = flip (I.findWithDefault 0) input

readMem' :: Mem -> Mem -> Addr -> Addr -> Dat
readMem' oldMem newMem c i | i < c = (IA.!) newMem i
                           | otherwise = (IA.!) oldMem i
          
runInstr :: (Addr -> Dat) -> (Addr -> Dat) -> (Int, Instr) -> State Status (Dat, Maybe (Addr, Dat))
runInstr readInput readMem (c, (SType sop addr))
    = let v1 = readMem addr
          vc = readMem c
          input = readInput addr
      in case sop of 
           Noop -> return (vc, Nothing)
           Cmpz f -> do put (f v1 0)
                        return (vc, Nothing)
           Sqrt -> return (sqrt v1, Nothing)
           Copy -> return (v1, Nothing)
           Input -> return (input, Nothing)
runInstr _ readMem (c, (DType dop addr1 addr2))
    = let v1 = readMem addr1
          v2 = readMem addr2
          vc = readMem c
      in case dop of
           Add -> return ((v1 + v2), Nothing)
           Sub -> return (v1 - v2, Nothing)
           Mult-> return (v1 * v2, Nothing)
           Div -> return ((if v2 == 0.0 then 0.0 else v1 / v2), Nothing)
           Output -> return (vc, Just (addr1, v2))
           Phi -> do z <- get
                     return (if z then v1 else v2, Nothing)

---- Dauern gerade zu lange.

main = do
  args <- getArgs
  when (length args < 1) $ do fail "\nUsage: vm binary\n\t first line \"16000 confignummer\"\n\tproceed with . \\n \n"; 
  let file = args !! 0
  B.readFile file >>= console' . loadVM 
--  let vm' = oneRun vm
--  print vm'
--  let vm'' = take 100 $ iterate oneRun vm
--  print vm''

 -- 16000 1001
  
  