{-# OPTIONS -XFlexibleInstances #-}
module Compile where

import Analyze
import Types

import System.IO
import System
import Control.Monad

import Console
import VM
import qualified Data.Set as S
import Data.Char

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Load
import GHC.IOBase
import System.Cmd
import System.Process
import Distribution.Simple.Utils

import Data.Function

import Debug.Trace
import Data.Maybe

-- LoC -> Line of Code
data LoC = LoC Addr Instr deriving (Ord, Eq)

class Compile a where
    compile :: a -> String -> String

instance (Compile a) => Compile [a] where
    compile code inner = foldr compile inner code    

instance Compile LoC where
    compile (LoC c  (SType sop addr)) inner
        = let v1 = compile (NMem addr) undefined
              vc = compile (NMem c) undefined
              input = --compile (NIn addr) undefined
                      nest ("I.findWithDefault 0 " ++ show addr ++" input")

          in case sop of 
               Noop 
                   -> inner
               Cmpz f
                   -> cLet "z" (compile f v1)
                      inner
               Sqrt 
                   -> cLet vc ("sqrt " ++ v1)
                      inner
               Copy 
                   -> cLet vc v1
                      inner
               Input 
                   -> cLet vc input
                      inner

    compile (LoC c (DType dop addr1 addr2)) inner
        = let v1 = compile (NMem addr1) undefined
              v2 = compile (NMem addr2) undefined
              vc = compile (NMem c) undefined
                  --compile (NOut addr1) undefined
              
          in case dop of
               Add -> cLet vc (v1++" + "++ v2)
                      inner
               Sub -> cLet vc (v1++" - "++ v2)
                      inner
               Mult-> cLet vc (v1++" * "++ v2)
                      inner
               Div -> cLet vc (nest ("if "++v2++" == 0.0 then 0.0 else "++v1++" / "++v2))
                      inner
               Output
                   -> cLet "outputTemp" (nest ("I.insert "++show addr1++" "++v2++" output"))
                      (cLet "output" "outputTemp"
                       inner)
               Phi -> cLet vc ("if z then "++v1++" else "++v2) 
                      inner
             

nest s = "("++s++")"
cLet senke quelle inner = nest ("let "++senke++" = " ++ quelle
                                ++ " in " ++ inner)

instance Compile CmpFun where
    compile f inner = nest (inner++show f)

instance Compile NameSpace where
    compile f _ = map toLower $ show f

instance Compile (String,VM) where
    compile (name,vm) undefined = src -- ++ "\n"++minstate
        where minstate_raw = "minstate_raw"
              src_raw = "module Temp where \nf "++minstate_raw++" = "++(compile locs minstate_raw)

              minstate = askGHCMinstate src_raw
              src = "module "++name++"(f) where \n"++
                    "import Data.IntMap as I\n"++
                    "import Types\n"++
                    "output = I.empty\n"++
                    "f "++nest (minstate++", Inp input")++" = "++(compile (map (uncurry LoC) (instr vm))
                                                               (nest (minstate ++", Outp output")))

              locs = (map (uncurry LoC) (instr vm))

-- (VM{  instr  :: [(Addr,Instr)] 
--    ,  mem    :: IntMap Dat
--   ,  status :: !Bool
--   ,  size   :: Int
--   ,  time   :: Int
--   } 

outputs :: [LoC] -> S.Set NameSpace
outputs = S.fromList . catMaybes . map output
    where output (LoC _ (DType Output addr1 _)) = Just (NOut addr1)
          output _ = Nothing

inputs :: [LoC] -> S.Set NameSpace
inputs = S.fromList . catMaybes . map output
    where output (LoC _ (SType Input addr)) = Just (NIn addr)
          output _ = Nothing

constants :: [LoC] -> S.Set NameSpace
constants = S.fromList . catMaybes . map constant
    where constant (LoC c (SType Noop _)) = Just (NMem c)
          constant (LoC c (SType (Cmpz _) _)) = Just (NMem c)

          constant (LoC c (DType Output _ _)) = Just (NMem c)

askGHCMinstate :: String -> S.Set [NameSpace]
askGHCMinstate src
    = unsafePerformIO
      (do writeFileAtomic "Temp.hs" src
          system ("ghc Temp.hs 2> temp_err_raw")
          system ("grep \"Not in scope\" temp_err_raw | grep -i mem > temp_err")
          l <- readFile "temp_err"
          return $ nest (concat . L.intersperse ", " . L.nub . L.sortBy cmp
                         . map (takeWhile (/='\''). tail.dropWhile (/='`'))
                         . lines $ l))

    where cmp = (compare `on` extract)
          extract :: String -> Int
          extract = read . drop 4

doCodeCompile args = do
  let file = args !! 1
      name = args !! 2
  print name
  dat <- B.readFile file
  let vm = loadVM dat
--  let vm = testVM
  let 
      s = (compile (name,vm) undefined)
  writeFileAtomic (name++".hs") s
--  print (askGHCMinstate s)

--  print . length . getMinimalState $ dataflow
-- print . length . getMaximalState $ dataflow


runCompile = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: See source.\n"
  let g = f 1
  case args !! 0 of 
    "code" -> doCodeCompile args
    otherwise -> fail "\nUsage: See Source.\n"

--  print (g 2)
--  print (g 3)

f a = g
    where g b = (c,a + b)
          c = trace "bla" (2*a)


