module Main where

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Debug.Trace
import Util
import Types
import Load

import System
import Control.Monad

import Console
import VM


          

        
{- shell  :: [(Addr,Dat)] -> (VM) -> (VM, [(Addr,Dat)])
shell inputs vm = 
    let inp = setInputs inputs (Input I.empty)
        (vm', out) = oneRun inp vm
        Output outmap = out
        outs = I.toAscList $ outmap
    in (vm', outs)
-}

main = do
  args <- getArgs
  when (length args < 1) $ do fail "\nUsage: vm binary\n\t first line \"16000 confignummer\"\n\tproceed with . \\n \n"; 
  let file = args !! 0
  dat <- B.readFile file
  let vm  = loadVM dat
-- print vm
  console oneRun vm
--  let vm' = oneRun vm
--  print vm'
--  let vm'' = take 100 $ iterate oneRun vm
--  print vm''

 -- 16000 1001
  
  