module Main where

import Util
import Types
import Data.Word
import Data.Maybe
import qualified Data.IntMap as I
import Data.List
import System
import Control.Monad

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


parse :: String -> [Pairs]
parse cont = 
    let ls     = lines cont
        blocks = partitionAt (== ".") ls    
        cleaned= map (\b -> 
                          map readConsoleInput $ filter (not . isComment) b) blocks
    in cleaned



main :: IO ()
main  = do
  f <- getContents
  let os = parse f
  let maxAddr = foldr (\ ps ma -> max (maximum (map fst ps)) ma) 0 os      
  putStrLn $ "#C t " ++ (concat $ intersperse " " $ take (maxAddr) $ ["fuel", "sx", "sy"] ++ map (show) [4..])
  let ls = map (showLine maxAddr) $ zip [0..] os       
  putStr $ unlines ls
      where
        showLine :: Int -> (Int,Pairs) -> String
        showLine maxAddr (t,ps) =
            let pmap = I.fromList ps
                vals = (fromIntegral t) : (map (\ x -> I.findWithDefault 0 x pmap) [1..maxAddr])
            in concat $ intersperse " " $ map show vals
            
