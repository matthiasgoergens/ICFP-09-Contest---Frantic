{-# OPTIONS -XScopedTypeVariables -fglasgow-exts #-}
module Analyze where

import Load
import Util
import Types
import qualified Data.Map as M

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.Set as S

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
import qualified Data.Set as S2

import Test.QuickCheck
import NameSpace




--instance Show (Int->Bool) where
--    show 


data Vertice = Vertice Addr IType Dat deriving Show
data Arc = Arc Senke (DependType, Quelle) deriving Show

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



data Senke  = SMem {target :: Addr} | SOut {target :: Addr} | SZ {target :: Addr} deriving Show
data Quelle = QMem {src :: Addr} | QIn  {src :: Addr} | QZ {src :: Addr} deriving Show

newtype IType = IType (Either DOP SOP)


instance Show IType where
    show (IType (Left op)) = show op

    show (IType (Right (Cmpz _))) = "Cmpz"
    show (IType (Right op)) = show op



toIType :: Instr -> IType
toIType (DType op _ _) = IType $ Left op
toIType (SType op _) = IType $ Right op

data Dataflow = Dataflow [Vertice] [Arc] deriving Show

analyzeDataflow :: VM -> Dataflow
analyzeDataflow vm = Dataflow vertices (arcs++arcsZ)
    where code = instr vm
          vertices = map calcV1 code
    
          calcV1 (addr, instr) = Vertice addr (toIType instr) (readMem vm addr)

          arcs = analyzeOrdinaryArcs code -- analyze' (I.fromList)
          arcsZ = analyzeZArcs code 

-- Arc Senke (DependType, Quelle)

analyzeZArcs :: [(Addr,Instr)] -> [Arc]
analyzeZArcs = concat . map go
               . splitBy (isCmp . snd)
               . rot (isCmp.snd) . filter (isPhiCmp.snd)
    where go :: ((Addr, Instr), [(Addr, Instr)]) -> [Arc]
          go (cmp, phis) = map (go1 cmp) phis
          go1 :: (Addr, Instr) -> (Addr, Instr) -> Arc
          go1 (cCmp, (SType (Cmpz _) addr)) (cPhi, (DType Phi addr1 addr2))
              = Arc (SMem cPhi) (If,QZ cCmp)


splitBy :: (a -> Bool) -> [a] -> [(a,[a])]
splitBy _ [] = []
splitBy pred l@(x:xs)
    | not . pred $ x
        = splitBy pred $ dropWhile (not . pred) xs
    | otherwise 
        = (x,init) : splitBy pred tail
    where (init, tail) = span (not . pred) xs

prop_SplitByConserve (Blind pred) (l::[Int])
    = any pred l ==>
      (dropWhile (not . pred) l
       == (concat . map fuse . splitBy pred $ l))
    where fuse (a, l) = a : l

prop_SplitByConserve2 (Blind pred) (l::[Int])
    = any pred l ==>
      (l'
       == (concat . map fuse . splitBy pred $ l'))
    where fuse (a, l) = a : l
          l' = rot pred l


-- L.groupBy ((==) `on` (isPhi.snd))

takeNWhile n pred l = init ++ takeWhile (pred) tail
    where (init, tail) = splitAt n l

prop_rot_idem (Blind pred) (l::[Int]) = rot pred (rot pred l) == rot pred l
prop_rot_conserve (Blind pred) (l::[Int]) = L.sort (rot pred l) == L.sort l

prop_rot_headTrue (Blind pred) (NonEmpty (l::[Int])) = any pred l
                                                       ==> h
    where h = pred ( head (rot pred l))

-- make Cmp come first!, if any
rot :: (a->Bool) -> [a] -> [a]
rot pred l = (rest ++ phis)
    where (phis, rest) = break (pred) l 

isPhiCmp t = isPhi t || isCmp t
isPhi (DType Phi _ _) = True
isPhi _ = False
isCmp (SType (Cmpz _) _) = True
isCmp _ = False

analyzeOrdinaryArcs :: [(Addr,Instr)] -> [Arc]
analyzeOrdinaryArcs = concat . map (uncurry go)
    where go c (DType Output addr1 addr2) = [Arc (SOut addr1) (Kopie, QMem addr1)]
          --(NOut addr1, [NMem addr2])

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
    where go (c, DType Output addr1 addr2) = (NOut addr1, [NMem addr2])
          go (c, DType op addr1 addr2) = (NMem c, [NMem addr1, NMem addr2])
          go (c, SType Noop _) = (NMem c, [])
          go (c, SType Input addr) = (NMem c, [NIn addr])
          go (c, SType op addr) = (NMem c, [NMem addr])






{-
-}


instance ToDot Dataflow where
    toDot (Dataflow vertices arcs) =
        "digraph dataflow {\n"
        ++ (concat . map toDot $ vertices)
        ++ "\n#Arcs:\n\n"
        ++ (concat . map toDot $ arcs)
        ++ "}\n"

-- data Vertice = Vertice Addr IType Dat
-- data Arc = Arc Senke (DependType, Quelle)

-- newtype IType = IType (Either DOP SOP)

-- data DOP  = Add | Sub | Mult | Div | Output | Phi deriving (Read, Show)
-- data SOP  = Noop | Cmpz (Dat -> Dat -> Bool) | Sqrt | Copy | Input deriving (Show)

-- data Senke  = SMem Addr | SOut Addr | SZ Addr
-- data Quelle = QMem Addr | QIn  Addr | QZ Addr



instance ToDot Vertice where
    toDot (Vertice addr itype dat) = 
        "node [shape = "++shape name++", label=\"" 
                       ++ show itype++"\\n"
                       -- ++ show name++"\\n"
                       ++show addr++"\\n"
                       ++show dat++"\"]; "++ toDot name ++";\n"
            where name = toNameSpace itype addr
                  shape (NMem _) = "box"
                  shape (NOut _) = "triangle"
                  shape (NIn _) = "triangle"
                  shape (NZ _) = "circle"
                                     -- ++ show dat ++"_"++ show addr ++ ";\n"

toNameSpace :: IType -> Addr -> NameSpace
toNameSpace (IType (Left op)) = case op of
                                  Add  -> NMem
                                  Sub  -> NMem
                                  Mult -> NMem
                                  Output -> NOut
                                  Div  -> NMem
                                  Phi  -> NMem

toNameSpace (IType (Right op)) = case op of
                                   Noop -> NMem
                                   Cmpz _ -> NZ
                                   Sqrt -> NMem
                                   Copy -> NMem
                                   Input -> NMem



forwardP (Arc senke (_,quelle)) = src quelle <= target senke

getMinimalState :: Dataflow -> [Quelle]
getMinimalState (Dataflow vertices arcs)
    = map getQuelle . filter (not . forwardP) $  arcs
    where getQuelle (Arc _ (_, quelle)) = quelle

getMaximalState :: Dataflow -> [Quelle]
getMaximalState (Dataflow vertices arcs)
    = map getQuelle . filter (const True) $  arcs
    where getQuelle (Arc _ (_, quelle)) = quelle

instance ToDot Arc where
    toDot arc@(Arc senke (dependType, quelle))
        = "edge[style="++style isForward++", dir="++dir isForward
          ++" arrowhead=\""++arrowhead dependType++"\""
          ++" contraint=\""++(show . not $ isForward)++"\""
          ++"]; "
          ++ q ++ " -> "++ s ++ ";\n"
              where style True =  "solid"
                    style False = "dashed"
                    dir _ = "forward"
                    dir False = "back"
                    isForward = forwardP arc

                    arrowhead Plus = "normal"
                    arrowhead Minus = "empty"
                    arrowhead Mal = "normal"
                    arrowhead Durch = "empty"
                    arrowhead Wurzel = "normal"
                    arrowhead Vergleich = "odot"
                    arrowhead If = "obox"
                    arrowhead Then = "normal"
                    arrowhead Else = "empty"
                    arrowhead Kopie = "normal"
                    (q,s) = case isForward of
                              _ -> (toDot quelle, toDot senke)
                              False -> (toDot senke, toDot quelle)

-- ++ show dependType

instance ToDot Senke where
    toDot (SMem addr) = toDot (NMem addr) -- "Mem_"++show addr
    toDot (SOut addr) = toDot (NOut addr) -- "Out_"++show addr
    toDot (SZ addr) = toDot (NZ addr) -- "Z_"++show addr

instance ToDot Quelle where
    toDot  = toDot . quelleToNamespace


quelleToNamespace (QMem addr) = (NMem addr) -- "Mem_"++show addr
quelleToNamespace (QIn addr) = (NIn addr) -- "In_"++show addr
quelleToNamespace (QZ addr) = (NZ addr) -- "Z_"++show addr

-- labelInp name = "node [shape = box, label=" ++ show name++"];" ++ show name ++";\n"

{-
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
                         ++ (concat . map labelInp . filter isNIn $ names) ++ "\n"
                         ++ (concat . map labelOutp . filter isNOut $ names) ++ "\n"






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
                               
-}
                               
invert :: [(NameSpace, [NameSpace])] -> [(NameSpace, [NameSpace])]
invert l = M.toList . M.fromListWith (++) . concat . map go $ l
    where go (sink, sources) = map (flip (,) [sink]) sources

isNMem (NMem _) = True
isNMem _ = False

isNIn (NIn _) = True
isNIn _ = False

isNOut (NOut _) = True
isNOut _ = False

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

testVM = VM {  instr  = ins 
            ,  mem    = I.fromList (take s (zip [0..] [0..]))
            ,  status = False
            ,  size   = 3 
            ,  time   = 0
            }
    where s = length ins
          ins = zip [0..]
                [SType Copy 1
                ,SType Noop 0
                ,SType Copy 1]
--                 [SType Input 0
--                 ,DType Add 0 7
--                 ,SType Noop 0
--                 ,DType Add 1 2
--                 ,SType (Cmpz (<=)) 1
--                 ,SType (Cmpz (<=)) 0
--                 ,SType Noop 0
--                 ,DType Phi 1 2
--                 ]

fullNetworkAnalysis args = do
  let file = args !! 1
  dat <- B.readFile file
  let vm = loadVM dat
--  let vm = testVM
  let dataflow = analyzeDataflow $ vm
  
--  putStr . toDot $ dataflow
--  print . getMinimalState $ dataflow
  putStrLn . toDot $ dataflow
--  print . length . getMinimalState $ dataflow
--  print . length . getMaximalState $ dataflow


compileLightningVM :: String -> VM -> String  
compileLightningVM name vm = "module " ++ name
    where
      dataflow = analyzeDataflow $ vm
      state = getMinimalState $ dataflow
      


{-
dependencyAnalysis args = do
  let file = args !! 1
  when (length args < 3) $ do fail "\nWhat do you want to analyses?\n"
  dat <- B.readFile file
  let vm = loadVM dat
  let n = (NOut . read $ (args !! 2))
  putStr $ showAnalysis vm $ analyseDepend (vm) (map NOut [2, 3])
-}


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


runAnalysis = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: See source.\n"
  case args !! 0 of 
    "full" -> fullNetworkAnalysis args
--    "dep" -> dependencyAnalysis args
--    "plot" -> gnuplotter_wrapper args
    otherwise -> fail "\nUsage: See Source.\n"