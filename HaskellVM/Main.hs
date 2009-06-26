module Main where

import qualified Data.ByteString as B
import Data.Array as Array
import Types

loadLine :: Int -> [Word8] -> (Instr, Dat)
loadLine i ds | odd  i =
    let ins = take 4 ds
        dat = drop 4 ds
        in (loadInstr ins dat)
loadLine i ds | even i =
    let dat = take 8 ds
        ins = drop 8 ds
        in (loadInstr ins dat)

loadInstrDat :: [Word8] -> [Word8] -> (Instr,Dat)
loadInstrDat ins dat = 
    let i1 = ins !! 3
        in (case i1 .&. 0xF0 == 0 of
              True  -> loadSType ins
              False -> loadDType ins
           , dat)

loadIMM :: Int -> (Dat -> Dat -> Bool)
loadIMM 0 = (<) 
loadIMM 1 = (<=)
loadIMM 2 = (==)
loadIMM 3 = (>=)
loadIMM 4 = (>)
loadIMM _ = Error "Wrong Imm code"
           
loadSType :: [Word8] -> Intr
loadSType (i4:i3:i2:i1:_) = 
    let op = (i1) .&. 0x0F
        imm = (i2) .&. 0xF0 `shift` (-4)
        r1  = i4 + (i3 .&. 0x3F) `shift` (8) 
        in case op of
             0 -> SOP Noop 0
             1 -> SOP Cmpz (loadIMM imm) r1
             2 -> SOP Sqrt r1
             3 -> SOP Copy r1
             4 -> SOP Input r1
             _ -> Error "Wrong SType OP"
     
loadDType :: [Word8] ->  Intr
loadDType (i4:i3:i2:i1:_) = 
    let op = i1 .&. 0xF0 `shift` (-4)
        r1 = i1 .&. 0x0F `shift` (10)
             + i2 `shift` 2 
             + i3 .&. 0x10 `shift` (-6)
        r2  = i4 + (i3 .&. 0x3F) `shift` (8) 
        in case op of
             0 -> DOP Add 
             1 -> DOP Cmpz (loadIMM imm) r1
             2 -> DOP Sqrt r1
             3 -> DOP Copy r1
             4 -> DOP Input r1
             _ -> Error "Wrong SType OP"
     

loadVM :: C.ByteString -> VM
loadVM dat = 
    let line = B.unpack $ B.take 12 dat
        

main = do
  args <- getArgs
  let file = args !! 0
  dat <- liftM C.pack $ readFile file
  let vm  = loadVM dat