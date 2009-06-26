module Util where
   
import qualified Data.ByteString as B
import qualified Data.List as L


partitionN :: Int -> B.ByteString -> [B.ByteString]
partitionN n b = L.unfoldr (helper) b
    where helper x | B.null x = Nothing
                   | otherwise = Just $ splitat n x
    