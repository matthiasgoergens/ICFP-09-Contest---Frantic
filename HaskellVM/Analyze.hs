module Main where

import Load
import Util
import Types
import qualified Data.Map as M

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Debug.Trace
import Util
import Types
import Load

import System
import Control.Monad

import Console
import VM
import qualified Data.Set as S

data NameSpace = NInp Addr | NOutp Addr | NMem Addr deriving (Ord, Eq)

instance Show NameSpace where
    show (NInp x) = "NInp" ++ show x
    show (NOutp x) = "NOutp" ++ show x
    show (NMem x) = "NMem" ++ show x

analyze :: VM -> [(NameSpace, [NameSpace])]
analyze vm = map go . instr $ vm
    where go (c, DType Output addr1 addr2) = (NOutp addr1, [NMem addr2])
          go (c, DType _ addr1 addr2) = (NMem c, [NMem addr1, NMem addr2])
          go (c, SType Noop _) = (NMem c, [])
          go (c, SType Input addr) = (NMem c, [NInp addr])
          go (c, SType _ addr) = (NMem c, [NMem addr])



showAnalysis :: [(NameSpace, [NameSpace])] -> String
showAnalysis l = "digraph dataflow {\n"
                 ++ (shapes . S.toList . S.fromList $ (concat (map snd l) ++ map fst l))
                 ++ (concat . map (uncurry go) $ l)
                 ++ "}\n"
    where go sink = concat . map (goLine sink)
          goLine sink source = show source ++ " -> " ++ show sink ++ ";\n"
          shapes :: [NameSpace] -> String
          shapes names = "node [shape = circle];\n"
                         ++ (concat . map ((++" ").show) . filter isNMem $ names) ++ ";\n"
                         ++ "node [shape = box];\n"
                         ++ (concat . map ((++" ").show) . filter isNInp $ names) ++ ";\n"
                         ++"node [shape = diamond];\n"
                         ++ (concat . map ((++" ").show) . filter isNOutp $ names) ++ ";\n"

isNMem (NMem _) = True
isNMem _ = False

isNInp (NInp _) = True
isNInp _ = False

isNOutp (NOutp _) = True
isNOutp _ = False

-- LR_0 -> LR_2 [ label = "SS(B)" ];

main = do
  args <- getArgs
  when (length args < 1) $ do fail "\nUsage: vm binary\n\t first line \"16000 confignummer\"\n\tproceed with . \\n \n"; 
  let file = args !! 0
  dat <- B.readFile file
  putStr . showAnalysis . analyze . loadVM $ dat