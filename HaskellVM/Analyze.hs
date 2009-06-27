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

invert :: [(NameSpace, [NameSpace])] -> [(NameSpace, [NameSpace])]
invert l = M.toList . M.fromListWith (++) . concat . map go $ l
    where go (sink, sources) = map (flip (,) [sink]) sources

isNMem (NMem _) = True
isNMem _ = False

isNInp (NInp _) = True
isNInp _ = False

isNOutp (NOutp _) = True
isNOutp _ = False

-- LR_0 -> LR_2 [ label = "SS(B)" ];

analyseDepend :: VM -> NameSpace -> [(NameSpace, [NameSpace])]
analyseDepend vm start = filter (flip S.member reached . fst) network
    where network = invert . analyze $ vm
          networkM = M.fromList network


          reached = follow networkM S.empty [start]

follow :: Ord a => M.Map a [a] -> S.Set a -> [a] -> S.Set a
follow networkM exhausted [] = exhausted                    
follow networkM exhausted (n:ns) = let news = filter (not . flip S.member exhausted)
                                              . M.findWithDefault [] n $ networkM
                                   in follow networkM (S.insert n exhausted) (news ++ ns)

prop_followCycle a s = (L.sort $ S.toList (follow networkM S.empty a)) == L.sort (a:s)
    where types :: (Int, [Int])
          types = (a,s)
          networkM = (M.fromList (zip (a ++ init s) (map (:[]) s)))

fullNetworkAnalysis args = do
  let file = args !! 1
  dat <- B.readFile file
  putStr . showAnalysis . analyze . loadVM $ dat

dependencyAnalysis args = do
  let file = args !! 1
  when (length args < 3) $ do fail "\nWhat do you want to analyses?\n"
  dat <- B.readFile file
  let n = (NOutp . read $ (args !! 2))
  print $ analyseDepend (loadVM $ dat) n
--  putStr . showAnalysis $ analyseDepend (loadVM $ dat) n


main = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: See source.\n"
  case args !! 0 of 
    "full" -> fullNetworkAnalysis args
    "dep" -> dependencyAnalysis args
    otherwise -> fail "\nUsage: See Source.\n"