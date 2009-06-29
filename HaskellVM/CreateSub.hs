module Main where

import Util
import Types
import Data.Word
import Data.Maybe
import qualified Data.IntMap as I
import qualified Data.ByteString.Lazy as B
import Data.Binary
import System
import Control.Monad
import Data.Function
import Data.List
import Debug.Trace

import ParseInOut

data Header = Header Int Int
data Frame = Frame { step :: Int
                   , vals :: [MPair]
                   } deriving (Show)

teamID :: Int
teamID = 151

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




main :: IO ()
main  = do
  args <- getArgs
  when (length args < 3) $ do fail "\nUsage: createSub inputtrace outputtrace submission_dest\n"; 
  let (inpfile:outfile:file:_) = args
  inpf <- readFile inpfile
  outf <- readFile outfile
  let is = parseInput inpf
  let os = parseOutput outf
  let scenario = fromJust $ lookup 16000 $ trace (show (take 100 is)) (head is)
      header   = Header teamID (floor $ scenario)
      max      = length os
      isfilter = zipWith (I.union `on` I.fromList) is ([] : map (map (mapsnd (const 0))) is)  
      frames'  = take max $ map (\(i,o) -> Frame {step = i, vals = map MPair (I.toAscList o)} ) $ zip [0..] isfilter
--      frames'  = map (\ f  -> f { vals = filter ( (==16000) `on` fst) (vals f)}) frames
--      frames'  = map (\ f  -> f { vals = sort (vals f)}) frames
      cleared  = filter (\ Frame {vals = vs} -> length vs > 0) frames'
      sub = B.concat (encode header : 
                      (map encode ( cleared ++ [Frame {step = max, vals = []}])) )
  putStrLn $ "Scenario: " ++ show scenario
--  print is
  print cleared
  B.writeFile file sub 
  
     