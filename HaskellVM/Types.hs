{-# OPTIONS_GHC -XFlexibleInstances #-}
module Types where

import Data.Array
import Data.IntMap

type Time = Int
type Addr = Int
type Dat = Double

type Pos = (Dat,Dat)
type Vec = (Dat,Dat)

data Instr = DType DOP !Addr !Addr 
           |  SType SOP !Addr 
             deriving (Show, Ord, Eq)
data DOP  = Add | Sub | Mult | Div | Output | Phi deriving (Read, Show, Ord, Eq)
data SOP  = Noop | Cmpz CmpFun | Sqrt | Copy | Input deriving (Show, Ord, Eq)

data CmpFun = Less | LEq | Eq | MEq | More deriving (Ord, Eq)

instance Show CmpFun
    where 
      show Less = "<0"
      show LEq = "<=0"
      show Eq = "==0"
      show MEq = ">=0"
      show More = ">0"

-- instance Show (Dat -> Dat -> Bool) where
--    show = const "<cmp-fun>"

-- length and DNA part
data VM = VM {  instr  :: [(Addr,Instr)] 
             ,  mem    :: IntMap Dat
             ,  status :: !Bool
             ,  size   :: Int
             ,  time   :: Int
             } deriving (Show)

data Inp  = Inp (IntMap Dat ) deriving (Show)
data Outp = Outp (IntMap Dat) deriving (Show)

mkInp :: [(Addr,Dat)] -> Inp
mkInp = Inp . fromList

fromInp :: Inp -> IntMap Dat
fromInp (Inp p) = p

mkOutp :: [(Addr,Dat)] -> Outp
mkOutp = Outp . fromList

fromOutp :: Outp -> IntMap Dat
fromOutp (Outp p) = p