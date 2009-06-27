module Main where

import Util
import Types
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Data.Binary
import System
import Control.Monad

type Pairs  = [(Addr, Dat)]
data MPair = MPair (Addr, Dat) deriving (Show)

fromMPair :: MPair -> (Addr, Dat)
fromMPair (MPair a) = a

data Header = Header Int Int
data Frame = Frame { step :: Int
                   , vals :: [MPair]
                   } deriving (Show)

instance Binary Header where
    put (Header team scenario) = do
                         put (reverseWord32 0xCAFEBABE)
                         put $ reverseWord32 (fromIntegral $ team)
                         put $ reverseWord32 (fromIntegral $ scenario)
    get = return $ Header 0 0 

instance Binary Frame where
    put (Frame { step = s, vals = vs}) = do
                        put (reverseWord32 $ fromIntegral $ s)
                        put (reverseWord32 $ fromIntegral $ (length vs))
                        mapM_ put $ vs
    get = return $ Frame {step = 0 , vals = []}

instance Binary MPair where
    put (MPair (a,d)) = do
                        put (reverseWord32 $ fromIntegral $ a)
                        put (reverseWord64 $ doubleToWord64 $ d)                                               
    get = return $ MPair (0,0)        


teamID :: Int
teamID = 151

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
  args <- getArgs
  when (length args < 1) $ do fail "\nUsage: createSub submission_dest < input\n"; 
  let file = args !! 0
  f <- getContents
  let os = parse f
  let scenario = fromJust $ lookup 16000 (head os)
      header   = Header teamID (floor $ scenario)
      frames   = map (\(i,o) -> Frame {step = i, vals = map MPair o} ) $ zip [0..] os 
      max      = length frames
      cleared  = filter (\ Frame {vals = vs} -> length vs > 0) frames  
      sub = B.concat (encode header : 
                      (map encode ( cleared ++ [Frame {step = max, vals = []}])) )
  putStrLn $ "Scenario: " ++ show scenario
--  print os
  print cleared
  B.writeFile file sub 
  
       