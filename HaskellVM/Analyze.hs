{-# OPTIONS -XScopedTypeVariables -fglasgow-exts #-}
module Main where

import Load
import Util
import Types
import qualified Data.Map as M

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Data.Function

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

import Test.QuickCheck

data NameSpace = NInp Addr | NOutp Addr | NMem  Addr deriving (Ord, Eq)

instance Show NameSpace where
    show (NInp x) = "NInp" ++ show x
    show (NOutp x) = "NOutp" ++ show x
    show (NMem x) = "NMem_" ++ show x

--instance Show (Int->Bool) where
--    show 


data Vertice = Vertice Addr IType Dat
data Arc = Arc Senke (DependType, Quelle)

data DependType = Plus | Minus | Mal | Durch
                | Wurzel
                
--                | Raus
--                | Rein

                | Vergleich
                | If
                | Then
                | Else
                | Kopie
                  deriving (Show, Eq, Ord)



data Senke  = SMem Addr | SOut Addr | SZ Addr
data Quelle = QMem Addr | QIn  Addr | QZ Addr

newtype IType = IType (Either DOP SOP)


instance Show IType where
    show (IType (Left op)) = show op

    show (IType (Right (Cmpz _))) = "Cmpz"
    show (IType (Right op)) = show op



toIType :: Instr -> IType
toIType (DType op _ _) = IType $ Left op
toIType (SType op _) = IType $ Right op

type Dataflow = ([Vertice], [Arc])



analyzeDataflow :: VM -> Dataflow
analyzeDataflow vm = (vertices, arcs)
    where code = instr vm
          vertices = map calcV1 code
    
          calcV1 (addr, instr) = Vertice addr (toIType instr) (readMem vm addr)

          arcs = analyzeZArcs code ++ analyzeOrdinaryArcs code -- analyze' (I.fromList)

-- Arc Senke (DependType, Quelle)

analyzeZArcs :: [(Addr,Instr)] -> [Arc]
analyzeZArcs = concat . map go
               . splitBy (isCmp.snd)
               . rot (isCmp.snd) . filter (isPhiCmp.snd)
    where go :: ((Addr, Instr), [(Addr, Instr)]) -> [Arc]
          go (cmp, phis) = map (go1 cmp) phis
          go1 :: (Addr, Instr) -> (Addr, Instr) -> Arc
          go1 (cCmp, (SType (Cmpz _) addr)) (cPhi, (DType Phi addr1 addr2))
              = Arc (SMem cPhi) (If,QZ cCmp)


splitBy :: (a -> Bool) -> [a] -> [(a,[a])]
splitBy _ [] = []
splitBy pred (x:xs) | not . pred $ x = error "splitBy: List should start with predicate evals to True"
                    | otherwise = (x,init) : splitBy pred xs
                    where (init, tail) = span (not.pred) xs

-- L.groupBy ((==) `on` (isPhi.snd))

takeNWhile n pred l = init ++ takeWhile (pred) tail
    where (init, tail) = splitAt n l

prop_rot_idem (Blind pred) (l::[Int]) = rot pred (rot pred l) == rot pred l
prop_rot_conserve (Blind pred) (l::[Int]) = L.sort (rot pred l) == L.sort l

prop_rot_headTrue (Blind pred) (NonEmpty (l::[Int])) = collect (zip l (map pred l)) h
    where h = pred ( head (rot pred l))

-- make Cmp come first!
rot :: (a->Bool) -> [a] -> Just [a]
rot pred l | null rest = Nothing
           | oterwile = Just (rest ++ phis)
    where (phis, rest) = break (pred) l 

isPhiCmp t = isPhi t || isCmp t
isPhi (DType Phi _ _) = True
isPhi _ = False
isCmp (SType (Cmpz _) _) = True
isCmp _ = False

analyzeOrdinaryArcs :: [(Addr,Instr)] -> [Arc]
analyzeOrdinaryArcs = concat . map (uncurry go)
    where go c (DType Output addr1 addr2) = [Arc (SOut addr1) (Kopie, QMem addr1)]
          --(NOutp addr1, [NMem addr2])

          go c (DType op addr1 addr2) = map (Arc (SMem c))
                                        (case op of
                                           Add  -> [(Plus, QMem addr1),(Plus, QMem addr2)]
                                           Sub  -> [(Plus, QMem addr1),(Minus, QMem addr2)]
                                           Mult -> [(Mal, QMem addr1),(Mal, QMem addr2)]
                                           Output -> error "Output should have been handled above."
                                           Div  -> [(Mal, QMem addr1),(Durch, QMem addr2)]
                                           Phi  -> [(Then, QMem addr1),(Else, QMem addr2)]
                                           
                                        )
          
          go c (SType (Cmpz _) addr) = [Arc (SZ c) (Vergleich, QMem addr)]
          go c (SType op addr) = map (Arc (SMem c))
                                 (case op of
                                    Noop -> []
                                    Cmpz _ -> error "Cmpz should have been handled above.\n"
                                    Sqrt -> [(Wurzel, QMem addr)]
                                    Copy -> [(Kopie, QMem addr)]
                                    Input -> [(Kopie, QIn addr)]
                                 )

analyze :: VM -> [(NameSpace, [NameSpace])]
analyze vm = (map go . instr $ vm) -- ++ phi_cmp (instr vm)
    where go (c, DType Output addr1 addr2) = (NOutp addr1, [NMem addr2])
          go (c, DType op addr1 addr2) = (NMem c, [NMem addr1, NMem addr2])
          go (c, SType Noop _) = (NMem c, [])
          go (c, SType Input addr) = (NMem c, [NInp addr])
          go (c, SType op addr) = (NMem c, [NMem addr])






{-
-}


showDataflow :: Dataflow -> String
showDataflow (vertices, arcs)
    = "digraph dataflow {\n"
      ++ (concat . map showVertice $ vertices)
      ++ (concat . map showArc $ arcs)
      ++ "}\n"

-- data Vertice = Vertice Addr IType Dat
-- data Arc = Arc Senke (DependType, Quelle)

-- newtype IType = IType (Either DOP SOP)

-- data DOP  = Add | Sub | Mult | Div | Output | Phi deriving (Read, Show)
-- data SOP  = Noop | Cmpz (Dat -> Dat -> Bool) | Sqrt | Copy | Input deriving (Show)

-- data Senke  = SMem Addr | SOut Addr | SZ Addr
-- data Quelle = QMem Addr | QIn  Addr | QZ Addr


showVertice :: Vertice -> String
showVertice (Vertice addr itype dat) = show itype ++ show dat ++ show addr ++ ";\n"

showArc :: Arc -> String
showArc (Arc senke (dependType, quelle)) = showSenke senke ++ " -> "++ showQuelle quelle ++ ";\n"
-- ++ show dependType

showSenke (SMem addr) = "Mem"++show addr
showSenke (SOut addr) = "Out"++show addr
showSenke (SZ addr) = "Z"++show addr
showQuelle (QMem addr) = "Mem"++show addr
showQuelle (QIn addr) = "In"++show addr
showQuelle (QZ addr) = "Z"++show addr


-- labelInp name = "node [shape = box, label=" ++ show name++"];" ++ show name ++";\n"

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






          labelInp name = "node [shape = box, label=" ++ show name++"];" ++ show name ++";\n"
          labelOutp name = "node [shape = diamond, label=" ++ show name++"];" ++ show name ++";\n"


--          labelNMem name = case snd (instr vm !! addr)
-- "node [shape = circle, label=" ++ show name++"_" ++ typeShow name++"];" ++ show name ++";\n"

          labelNMems names = (concat . map labelNMem . filter isNMem $ names)
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

oprop_followCycle a s' = -- trace (show l ++ "\n") $
    (l 
    == (L.nub $ L.sort (a : s)))
    where types :: (Int, [Int])
          types = (a,s)
          networkM = (M.fromList (zip ([a] ++ init s) (map (:[]) s)))
          l = L.sort $ S.toList (follow networkM S.empty [a])
          s = L.nub . L.sort $ s'

oprop_follow = follow (M.fromList [(1,[2]), (2,[3]), (4,[5])]) S.empty [1] == S.fromList [1,2,3]

fullNetworkAnalysis args = do
  let file = args !! 1
  dat <- B.readFile file
  let vm = loadVM dat
  putStr . showDataflow . analyzeDataflow $ vm

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