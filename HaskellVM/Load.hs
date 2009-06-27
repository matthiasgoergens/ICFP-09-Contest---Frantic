module Load where

import qualified Data.ByteString as B
import Data.Array as Array
import Types
import Data.Word
import Data.Bits
import qualified Data.IntMap as IntMap
import Util
import Control.Monad

loadLine :: Int -> [Word8] -> (Instr, Dat)
loadLine i ds | odd  i =
    let ins = take 4 ds
        dat = drop 4 ds
    in (loadInstrDat ins dat)

loadLine i ds | even i =
    let dat = take 8 ds
        ins = drop 8 ds
    in (loadInstrDat ins dat)

loadInstrDat :: [Word8] -> [Word8] -> (Instr,Dat)
loadInstrDat ins dat = 
    let i1 = ins !! 3
        in (case (i1 .&. 0xF0) == 0 of
              True  -> loadSType ins
              False -> loadDType ins
           , convertToDouble dat )

loadIMM :: Word8 -> (Dat -> Dat -> Bool)
loadIMM 0 = (<) 
loadIMM 1 = (<=)
loadIMM 2 = (==)
loadIMM 3 = (>=)
loadIMM 4 = (>)
loadIMM _ = error "Wrong Imm code"
           
loadSType :: [Word8] -> Instr
loadSType (i4:i3:i2:i1:_) = 
    let op = (i1) .&. 0x0F
        imm = (i2 .&. 0xF0) `shift` (-4)
        r1  = fromIntegral $ i4 + (i3 .&. 0x3F) `shift` (8) 
        in case op of
             0 -> SType Noop 0
             1 -> SType (Cmpz (loadIMM imm)) r1
             2 -> SType Sqrt r1
             3 -> SType Copy r1
             4 -> SType Input r1
             _ -> error $ "Wrong SType " ++ (show op)
     
loadDType :: [Word8] ->  Instr
loadDType (i4:i3:i2:i1:_) = 
    let op = (i1 .&. 0xF0) `shift` (-4)
        r1 = fromIntegral $ 
             (i1 .&. 0x0F) `shift` (10)
             + i2 `shift` 2 
             + (i3 .&. 0x10) `shift` (-6)
        r2  = fromIntegral $ 
              i4 + (i3 .&. 0x3F) `shift` (8) 
        in case op of
             1 -> DType Add r1 r2 
             2 -> DType Sub r1 r2
             3 -> DType Mult r1 r2
             4 -> DType Div  r1 r2
             5 -> DType Output r1 r2
             6 -> DType Phi r1 r2
             _ -> error $"Wrong DType " ++ (show op)
     

loadVM :: B.ByteString -> VM
loadVM dat =
    let ls :: [(Int,B.ByteString)]
        ls = zip [0..] (partitionN 12 dat) 
        instdat = map (uncurry loadLine) $
                  map (\(i,x) -> (i,B.unpack x)) ls
        insts    = map fst instdat
        dats     = map snd instdat
        max      = length dats
    in VM { instr  = zip [0..] insts
          , mem    = IntMap.fromAscList $ zip [0..] dats
          , ip     = 0
          , status = False
          , size   = max
          , input  = IntMap.empty
          , output = IntMap.empty
          }
