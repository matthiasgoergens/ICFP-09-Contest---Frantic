module Main where

import Util
import Types
import Data.Word
import Data.Maybe
import qualified Data.IntMap as I
import Data.List
import System
import Control.Monad

import ParseInOut


main :: IO ()
main  = do
  f <- getContents
  let os = parseOutput f
  let maxAddr = maximum (map fst (head os)) -- foldr (\ ps ma -> max (maximum (map fst ps)) ma) 0 os      
  putStrLn $ "#C t " ++ (concat $ intersperse " " $ take (maxAddr) $ ["fuel", "sx", "sy"] ++ map (show) [4..])
  let ls = map (showLine maxAddr) $ zip [0..] os       
  putStr $ unlines ls
      where
        showLine :: Int -> (Int,Pairs) -> String
        showLine maxAddr (t,ps) = concat $ intersperse " " 
                                  $ (show $ fromIntegral t) : (map (show . snd) $ drop 1 ps)
{-        showLine :: Int -> (Int,Pairs) -> String
        showLine maxAddr (t,ps) =
            let pmap = I.fromList ps
                vals = (fromIntegral t) : (map (\ x -> I.findWithDefault 0 x pmap) [1..maxAddr])
            in concat $ intersperse " " $ map show vals
-}
            
