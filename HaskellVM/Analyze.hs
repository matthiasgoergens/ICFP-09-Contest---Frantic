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

import System.IO
import System
import Control.Monad

import Console
import VM
import qualified Data.Set as S

data NameSpace = NInp Addr | NOutp Addr | NMem  Addr deriving (Ord, Eq)

instance Show NameSpace where
    show (NInp x) = "NInp" ++ show x
    show (NOutp x) = "NOutp" ++ show x
    show (NMem x) = "NMem_" ++ show x

    
analyze' :: VM -> [(NameSpace, [NameSpace])]
analyze' vm = map go . instr $ vm
    where go (c, DType Output addr1 addr2) = (NOutp addr1, [NMem addr2])
          go (c, DType op addr1 addr2) = (NMem c, [NMem addr1, NMem addr2])
          go (c, SType Noop _) = (NMem c, [])
          go (c, SType Input addr) = (NMem c, [NInp addr])
          go (c, SType op addr) = (NMem c, [NMem addr])

analyze :: VM -> [(NameSpace, [NameSpace])]
analyze vm = (map go . instr $ vm) ++ phi_cmp (instr vm)
    where go (c, DType Output addr1 addr2) = (NOutp addr1, [NMem addr2])
          go (c, DType op addr1 addr2) = (NMem c, [NMem addr1, NMem addr2])
          go (c, SType Noop _) = (NMem c, [])
          go (c, SType Input addr) = (NMem c, [NInp addr])
          go (c, SType op addr) = (NMem c, [NMem addr])

          isPhiCmp t = isPhi t || isCmp t
          isPhi (DType Phi _ _) = True
          isPhi _ = False
          isCmp (SType Cmpz _) = True
          isCmp _ = False

          -- start with cmp
          normalize z = rest ++ phis
              where (phis, rest) = break isCmp z

          phi_cmp instrs = op . normalize . filter (isPhiCmp.snd) inst

          op (


{-
data Instr = DType DOP !Addr !Addr 
           |  SType SOP !Addr 
             deriving (Show)
data DOP  = Add | Sub | Mult | Div | Output | Phi deriving (Read, Show)
data SOP  = Noop | Cmpz (Dat -> Dat -> Bool) | Sqrt | Copy | Input deriving (Show)
-}


showAnalysis :: VM -> [(NameSpace, [NameSpace])] -> String
showAnalysis vm l = "digraph dataflow {\n"
                 ++ (shapes . S.toList . S.fromList $ (concat (map snd l) ++ map fst l))
                 ++ (concat . map (uncurry go) $ l)
                 ++ "}\n"
    where go sink = concat . map (goLine sink)
--           goLine sink source = show source ++ " -> " ++ show sink ++ ";\n"

          goLine sink source                     
              | sink < source = "edge[style=dashed, dir=back]; "
                                ++show sink ++ " -> " ++ show source ++ ";\n"
              | otherwise = "edge[style=solid, dir=forward]; "
                            ++show source ++ " -> " ++ show sink ++ ";\n"

          shapes :: [NameSpace] -> String
          shapes names = labelNMems names ++ "\n"
                         ++ (concat . map labelInp . filter isNInp $ names) ++ "\n"
                         ++ (concat . map labelOutp . filter isNOutp $ names) ++ "\n"




          labelNMems names = (concat . map labelNMem . filter isNMem $ names)

          labelInp name = "node [shape = box, label=" ++ show name++"];" ++ show name ++";\n"
          labelOutp name = "node [shape = diamond, label=" ++ show name++"];" ++ show name ++";\n"


--          labelNMem name = case snd (instr vm !! addr)
-- "node [shape = circle, label=" ++ show name++"_" ++ typeShow name++"];" ++ show name ++";\n"
          labelNMem name@(NMem addr) = "node [shape = circle, label=\"" ++ show addr++"\\n" ++ typeShow addr
                                       ++"\\n"++show (readMem vm addr)++"\"];" ++ show name ++";\n"
          typeShow addr = case snd (instr vm !! addr) of
--                            SType Noop _ -> show (readMem vm addr)
                            DType t _ _ -> typeD t
                            SType t _ -> typeS t
                               
          typeD = show 

          typeS (Cmpz _) = "Cmpz"
          typeS t = show t
                               
                               


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



analyseDepend :: VM -> [NameSpace] -> [(NameSpace, [NameSpace])]
analyseDepend vm start = filter (flip S.member reached . fst) network
    where network = analyze $ vm
          networkM = M.fromList network
          reached = follow networkM S.empty start

follow :: Ord a => M.Map a [a] -> S.Set a -> [a] -> S.Set a
follow networkM exhausted [] = exhausted                    
follow networkM exhausted (n:ns) = let news = filter (not . flip S.member exhausted)
                                              . M.findWithDefault [] n $ networkM
                                   in follow networkM (S.insert n exhausted) (news ++ ns)

prop_followCycle a s' = -- trace (show l ++ "\n") $
    (l 
    == (L.nub $ L.sort (a : s)))
    where types :: (Int, [Int])
          types = (a,s)
          networkM = (M.fromList (zip ([a] ++ init s) (map (:[]) s)))
          l = L.sort $ S.toList (follow networkM S.empty [a])
          s = L.nub . L.sort $ s'

prop_follow = follow (M.fromList [(1,[2]), (2,[3]), (4,[5])]) S.empty [1] == S.fromList [1,2,3]

fullNetworkAnalysis args = do
  let file = args !! 1
  dat <- B.readFile file
  let vm = loadVM dat
  putStr . showAnalysis vm . analyze $ vm

dependencyAnalysis args = do
  let file = args !! 1
  when (length args < 3) $ do fail "\nWhat do you want to analyses?\n"
  dat <- B.readFile file
  let vm = loadVM dat
  let n = (NOutp . read $ (args !! 2))
  putStr $ showAnalysis vm $ analyseDepend (vm) (map NOutp [2, 3])


gnuplotter :: [Int] -> (Inp -> VM -> (VM, Outp)) -> VM -> IO()
gnuplotter outports oneRun vm = helper vm
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
        loop :: Inp -> VM -> IO (VM, Outp)
        loop inp vm = do
          let (vm', out) = oneRun inp vm 
              Outp outmap = out
              lookup :: Int -> Dat
              lookup k = (I.findWithDefault 0.0) k outmap
          putStrLn . ((++) (show (time vm') ++"\t")) . concat . map ((++"\t").show)
                     . map lookup $ outports
          hFlush stdout
          return (vm', out)

gnuplotter_wrapper args = do
  let file = args !! 1
  dat <- B.readFile file
  let vm  = loadVM dat
  gnuplotter [0,1,2,3,4] oneRun vm

main = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: See source.\n"
  case args !! 0 of 
    "full" -> fullNetworkAnalysis args
    "dep" -> dependencyAnalysis args
    "plot" -> gnuplotter_wrapper args
    otherwise -> fail "\nUsage: See Source.\n"