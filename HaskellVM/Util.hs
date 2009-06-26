module Util where
   
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Bits
import Data.Binary
import Control.Monad

partitionN :: Int -> B.ByteString -> [B.ByteString]
partitionN n b = L.unfoldr (helper) b
    where helper x | B.null x = Nothing
                   | otherwise = Just $ B.splitAt n x

convertToDouble :: [Word8] -> Double
--convertToDouble = word64ToDouble 
-- decode . BL.concat . map encode 

word64ToDouble :: Word64 -> Double 
-- word64ToDouble = unsafeReinterpret 
word64ToDouble = decodeIEEE 11 52    
 
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
    