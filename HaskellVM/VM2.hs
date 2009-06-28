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
    where oneStep input = do
            (statusO, memO) <- get
            let (statusN, memN, outPorts)
                    = oneRun' size code statusO memO input
            put (statusN, memN)
            return outPorts




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
                     outports = manyRuns (size vm) (instr vm) (status vm) (map2array (mem vm))
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

-- oneRun' :: Int -> [(Int, Instr)] -> Status -> Mem -> InPorts -> (Status, Mem, OutPorts)  

--    foldr (oneRun sive code)


-- oneRun (Inp inp) vm = (vm { mem = array2map newMem
--                           ,  status = newStatus
--                           ,  time = time vm + 1
--                           }
--                       , Outp outPorts)
--     where (newStatus, newMem, outPorts) = oneRun' (size vm) (instr vm) (status vm) (map2array (mem vm)) inp
    

prop_oneRun' = trace (show $ IA.elems mem) $
               and [(newStatus == False)
                   , I.null outports
                   , IA.elems mem == [5,8,13]]
    where (newStatus, mem, outports) =
              (oneRun' 3 [(0,SType Noop 0)
                         ,(1,SType Noop 0)
                         ,(2,DType Add 0 1)
                         ])
               False
               (IA.array (0,2) [(0,5)
                               ,(1,8)
                               ,(2,0)])
               (I.fromList [(0,1)
                           ,(1,2)
                           ,(2,3)
                           ]
               )
               

oneRun' :: Int -> [(Int, Instr)] -> Status -> Mem -> InPorts -> (Status, Mem, OutPort)
oneRun' size code oldStatus oldMem input = (newStatus, newMem, outPorts)
    where helper :: Int -> Instr -> Status -> (Status, Dat, OutPorts -> OutPorts)
          helper c (SType sop addr) z
              = -- trace ("c: "++show c) $
                let v1 = readMem c addr
                    vc = readMem c c
                    input = readInput addr
                in case sop of 
                     Noop -> (z, vc, id)
                     Cmpz f -> (f v1 0, vc, id)
                     Sqrt -> (z, sqrt v1, id)
                     Copy -> (z, v1, id)
                     Input -> (z, input, id)
          helper c (DType dop addr1 addr2) z
              = -- trace ("c: "++show c) $
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
          
--          op :: (Status -> (Status, Dat, OutPorts -> OutPorts))
--             -> (Status, (OutPorts -> OutPorts))
--             -> ((Status, (OutPorts -> OutPorts)) -> EndList Dat (Status, OutPorts -> OutPorts))
--             -> EndList Dat (Status, OutPorts -> OutPorts)

          op :: (Status -> (Status, Dat, OutPorts -> OutPorts))
             -> (Status, (OutPorts -> OutPorts))
             -> (Dat, (Status, OutPorts -> OutPorts))
          op f (oldStatus, oldOut)
              = (dat, (newStatus, (newOut . oldOut)))
              where (newStatus, dat, newOut) = f oldStatus
          endList :: EndList Dat (Status, OutPorts -> OutPorts)
          endList = foldEndList (oldStatus, id)
                    . L.map op . L.map (uncurry helper) . L.sortBy (compare `on` fst) $ code

          (newStatus, outPortsTrans) = end endList

          outPorts = outPortsTrans I.empty
          
          newMemPre = toList endList
          newMem = IA.array (0, size - 1) (zip [0..] newMemPre)

          readMem c i | i < c = (IA.!) newMem i
                      | otherwise = (IA.!) oldMem i

          readInput :: Int -> Dat
          readInput = flip (I.findWithDefault 0) input
          
-- (Status -> (Status, Dat, Outports -> Outports))

data EndList a b = Cons a (EndList a b) | Nil b deriving (Ord, Eq, Show)

splitEndList l = (toList l, end l)

end (Nil b) = b
end (Cons _ b) = end b
toList (Cons a b) = a : toList b
toList (Nil _) = []

---- Dauern gerade zu lange.

-- prop_toList a = (toList . foldr Cons (Nil undefined) $ a) == a
--     where types :: [Int]
--           types = a

-- prop_end a b = (end . foldr Cons (Nil b) $ map (const undefined) a) == b
--     where types :: ([Int], Int)
--           types = (a, b)

-- foldr'' :: (a -> b -> b) -> b -> [a] -> b
-- foldr'' op z [] = z
-- foldr'' op z (x:xs) = op x (foldr op z xs)

-- a = (z -> (z -> c) -> c)
-- b = z

-- foldl :: (a -> b -> a) -> a -> [b] -> a


--opCons :: [(Status, (OutPorts -> OutPorts))
--           -> EndList Dat (Status, OutPorts -> OutPorts)]

foldEndList :: z -> [z -> (a, z)] -> EndList a z
foldEndList z [] = Nil z
foldEndList oldZ (opCons : xs) = Cons dat $ foldEndList newZ xs
    where (dat, newZ) = opCons oldZ

     

-- prop_foldEndList a = (toList el == a)
--     where opCons = map opCon a
--           opCon i z = (i, z)
--           el = (foldEndList 0 opCons)
--           types :: [String]
--           types = a


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
  B.readFile file >>= console' . loadVM 
--  let vm' = oneRun vm
--  print vm'
--  let vm'' = take 100 $ iterate oneRun vm
--  print vm''

 -- 16000 1001
  
  