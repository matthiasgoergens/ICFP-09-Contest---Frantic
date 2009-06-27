{-# OPTIONS_GHC -XFlexibleInstances #-}
module Types where

import Data.Array
import Data.IntMap

type Addr = Int
type Dat = Double

data Instr = DType DOP !Addr !Addr 
           |  SType SOP !Addr 
             deriving (Show)
data DOP  = Add | Sub | Mult | Div | Output | Phi deriving (Read, Show)
data SOP  = Noop | Cmpz (Dat -> Dat -> Bool) | Sqrt | Copy | Input deriving (Show)

instance Show (Dat -> Dat -> Bool) where
    show = const "<cmp-fun>"

-- length and DNA part
data VM = VM {  instr  :: [(Int,Instr)] 
             ,  mem    :: IntMap Dat
             ,  ip     :: Int
             ,  status :: Bool
             ,  size   :: Int
             ,   input  :: IntMap Dat 
             ,   output :: IntMap Dat 
             } deriving (Show)