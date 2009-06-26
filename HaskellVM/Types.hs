module Types where

import Data.Array

type Addr = Int
type Dat = Double

data Instr = DOP !Addr !Addr 
           |  SOP !Addr 
             deriving (Read, Show)
data DOP  = Add | Sub | Mult | Div | Output | Phi deriving (Read, Show)
data SOP  = Noop | Cmpz (Dat -> Dat -> Bool) | Sqrt | Copy | Input deriving (Read, Show)

-- length and DNA part
dat VM = VM {  instr  :: Array Int Instr 
            ,  mem    :: Array Int Dat
            ,  ip     :: Int
            ,  status :: Bool
            } deriving (Read, Show)
