module Util where
   
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.IntMap as I
import Data.Bits
import Data.Binary
import Control.Monad

import Types

---- RUNNING
readMem ::   VM -> Addr -> Dat
readMem VM {mem = m}  addr = I.findWithDefault 0 addr m

writeMem :: Addr -> Dat -> VM ->VM 
writeMem addr v vm  = 
    vm { mem = I.insert addr v $ mem vm }

readInput :: Inp -> Addr -> Dat
readInput (Inp inp) addr = I.findWithDefault 0 addr inp 

{-
setInputs :: [(Addr, Dat)] -> VM -> VM
setInputs inps vm = 
    vm { input = foldr (\(a,d) map -> I.insert a d map) (input vm) inps }
-}

setInputs :: [(Addr, Dat)] -> Inp -> Inp
setInputs inps (Inp inp)  = 
    Inp (foldr (\(a,d) map -> I.insert a d map) inp inps )


writeOutput :: Addr -> Dat -> Outp -> Outp
writeOutput addr v (Outp out)  = 
    Outp $ I.insert addr v $ out


readConsoleInput :: String -> (Addr, Dat)
readConsoleInput s = let (a,d) = L.break (== ' ') s
                     in (read a, read d)

showConsoleOutput :: (Addr, Dat) -> String
showConsoleOutput (a,d) = show a ++ " " ++ show d

readConsoleLines :: IO([String])
readConsoleLines = do
  l <- getLine
  case head l of 
    '.' -> return []
    _   -> do ls <- readConsoleLines
              return (l:ls)


isFinished :: Outp -> Bool
isFinished (Outp o)
    = (I.findWithDefault 0 0 o) /= 0

score :: Outp -> Dat
score (Outp o) 
    = I.findWithDefault 0 0 o

---- LOADING
partitionN :: Int -> B.ByteString -> [B.ByteString]
partitionN n b = L.unfoldr (helper) b
    where helper x | B.null x = Nothing
                   | otherwise = Just $ B.splitAt n x

word8ToWord32 :: [Word8] -> Word32
word8ToWord32 ds = decode . BL.concat . map encode $ ds

convertToDouble :: [Word8] -> Double
convertToDouble ds = word64ToDouble . decode . BL.concat . map encode $ reverse ds

word64ToDouble :: Word64 -> Double 
-- word64ToDouble = unsafeReinterpret 
word64ToDouble = decodeIEEE 11 52    
 
-- TODO: Check if this works for denormalized numbers, NaNs and infinities. 
decodeIEEE :: (Bits a, Integral a, RealFloat b) => Int -> Int -> a -> b 
decodeIEEE exponentBits significandBits n = encodeFloat significand exponent 
   where significand = adjustSign (fromIntegral (adjustSignificand significandField)) 
         exponent = exponentField - exponentBias - significandBits 
 
         adjustSign = if n `testBit` (exponentBits + significandBits) then negate else id 
         adjustSignificand = if exponentField > 0 then (`setBit` significandBits) else id 
 
         exponentBias = bit (exponentBits - 1) - 1 
         exponentField = fromIntegral ((n `shiftR` significandBits) .&. exponentMask) 
         exponentMask = bit exponentBits - 1 
 
         significandField = n .&. significandMask 
         significandMask = bit significandBits - 1 
    

doubleToWord64 :: Double -> Word64 
doubleToWord64 = encodeIEEE 11 52 
 
-- TODO: Check if this works for denormalized numbers, NaNs and infinities. 
encodeIEEE :: (RealFloat a, Bits b, Integral b) => Int -> Int -> a -> b 
encodeIEEE exponentBits significandBits f = 
      (signBit `shiftL` (exponentBits + significandBits)) .|. 
      (exponentField `shiftL` significandBits) .|. 
      significandField 
   where (significand, exponent) = decodeFloat f 
 
         signBit | significand < 0 = 1 
                 | otherwise = 0 
         exponentField | significand == 0 && exponent == 0 = 0 
                       | otherwise = fromIntegral exponent + exponentBias + fromIntegral significandBits 
         significandField = fromIntegral (abs significand) .&. significandMask 
 
         exponentBias = bit (exponentBits - 1) - 1 
         significandMask = bit significandBits - 1 
 
