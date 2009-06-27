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
    let i32 = word8ToWord32 $ reverse ins
        in (case (i32 `shift` (-28)) == 0 of
              True  -> loadSType i32
              False -> loadDType i32
           , convertToDouble dat )

loadIMM :: Word32 -> (Dat -> Dat -> Bool)
loadIMM 0 = (<) 
loadIMM 1 = (<=)
loadIMM 2 = (==)
loadIMM 3 = (>=)
loadIMM 4 = (>)
loadIMM _ = error "Wrong Imm code"

loadSType :: Word32 -> Instr
loadSType i = 
    let op = (i `shift` (-24)) .&. 0x0F
        imm = (i `shift` (-21)) .&. 0x7
        r1  = fromIntegral $ (i .&. 0x3FFF)
        in case op of
             0 -> SType Noop 0
             1 -> SType (Cmpz (loadIMM imm)) r1
             2 -> SType Sqrt r1
             3 -> SType Copy r1
             4 -> SType Input r1
             _ -> error $ "Wrong SType " ++ (show op)

     
loadDType :: Word32 ->  Instr
loadDType i = 
    let op = ( i  `shift` (-28)) .&. 0xF
        r1 = fromIntegral $ 
             ( i  `shift` (-14)) .&. 0x3FFF
        r2 = fromIntegral $ 
             ( i .&. 0x3FFF)
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
          , status = False
          , size   = max
          }
