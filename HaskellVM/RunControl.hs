module Main where

import Types
import Controller
import ControllerUtils
import Util
import SimpleController
import Load

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.DList as DL
import Control.Monad
import System.IO
import System

main :: IO()
main = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: vm binary conf\n"; 
  let file = args !! 0
  let conf = read (args !! 1) :: Dat
  vm <- loadVMFromFile file
  let cont = case floor $ conf/1000.0 of
               1 -> task1Controller
               2 -> task2Controller
               _ -> error "not implemented"
  let trace = runController (cont conf) vm 
  sequence_ ( map showFrame $ trace)
    where showFrame (timeStep, inp, out) = do
            let inmap = fromInp inp
                outmap = fromOutp out
            putStrLn $ "#time: " ++ (show timeStep)
            putStr "#inp:"
            putStrLn $ concat $ L.intersperse "#" $ map showConsoleOutput $ I.toAscList inmap
            putStrLn "#out:"
            putStr $ unlines $ map showConsoleOutput $ I.toAscList outmap
            putStrLn "."
            hFlush stdout
