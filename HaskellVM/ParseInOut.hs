module ParseInOut where

import Types
import Util

type Pairs  = [(Addr, Dat)]
data MPair = MPair (Addr, Dat) deriving (Show)

fromMPair :: MPair -> (Addr, Dat)
fromMPair (MPair a) = a


partitionAt :: ( a-> Bool) -> [a] -> [[a]]
partitionAt f []   = []
partitionAt f list = 
    (takeWhile (not . f) list) : (partitionAt f $ drop 1 $ dropWhile (not . f) list)

isComment :: String -> Bool
isComment ('#':_) = True
isComment []      = True
isComment _       = False


parseInput :: String -> [Pairs]
parseInput cont = 
    let ls     = lines cont
        blocks = partitionAt (== ".") ls    
        cleaned= map (\b -> 
                          map readConsoleInput $ filter (not . isComment) b) blocks
    in cleaned

parseOutput :: String -> [Pairs]
parseOutput cont = 
    let ls     = lines cont
        blocks = partitionAt (== ".") ls    
        cleaned= map (\b -> 
                          map readConsoleInput $ filter (not . isComment) b) blocks
    in cleaned
