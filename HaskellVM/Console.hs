module Console where

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L

import Debug.Trace
import Util
import Types
import Load

import System
import System.IO
import Control.Monad

import VM

console :: (Inp -> VM -> (VM, Outp)) -> VM -> IO()
console oneRun vm = helper vm
      where           
        helper :: VM  -> IO()
        helper vm = do          
             ls <- readConsoleLines
             let inputdat   = map (readConsoleInput) ls
                 inp        = setInputs inputdat (Inp I.empty)
             (vm', out) <- loop inp vm
             case (isFinished out) of
               True  -> putStrLn $"Finished with score " ++ (show $ score out) 
               False -> helper vm'
        loop :: Inp -> VM -> IO(VM,Outp)
        loop inp vm = do          
          let (vm', out) = oneRun inp vm 
              Outp outmap = out
              Inp inmap   = inp
          putStrLn $ "#time: " ++ (show $ time vm')
          putStr "#inp:"  
          putStrLn $ concat $ L.intersperse "#" $ map showConsoleOutput $ I.toAscList inmap
          putStrLn "#out:"  
          putStr $ unlines $ map showConsoleOutput $ I.toAscList outmap
          putStrLn "."
          hFlush stdout
          return (vm', out)