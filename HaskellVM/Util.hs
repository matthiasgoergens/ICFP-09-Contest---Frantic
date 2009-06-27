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

readInput :: VM -> Addr -> Dat
readInput VM {input = inp} addr = I.findWithDefault 0 addr inp 

writeOutput :: Addr -> Dat -> VM ->VM 
writeOutput addr v vm  = 
    vm { output = I.insert addr v $ output vm }

---- LOADING
partitionN :: Int -> B.ByteString -> [B.ByteString]
partitionN n b = L.unfoldr (helper) b
    where helper x | B.null x = Nothing
                   | otherwise = Just $ B.splitAt n x

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
    