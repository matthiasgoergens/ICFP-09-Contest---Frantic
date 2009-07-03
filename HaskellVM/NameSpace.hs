module NameSpace where
import Types
import Text.Read

data NameSpace = NIn Addr | NOut Addr | NMem  Addr | NZ Addr deriving (Ord, Eq)
instance Show NameSpace where
    show (NIn x) = "inp_" ++ show x
    show (NOut x) = "out_" ++ show x
    show (NMem x) = "mem_" ++ show x
    show (NZ x) = "z_" ++ show x


class ToDot a where
    toDot :: a -> String

instance ToDot NameSpace where
    toDot = show