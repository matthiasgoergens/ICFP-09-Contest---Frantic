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


mkInputFile :: Trace -> String
mkInputFile trace = 
    let noCons = map ( \(Trace1 t (Inp m) _) -> (t, m)) trace
        imap = I.fromListWith (I.union) noCons
        max  = (maximum $ I.keys imap)
        allsteps = map (\t -> I.findWithDefault I.empty t imap) $ take max [0..]
    in unlines $ map showStep allsteps
       where showStep :: (I.IntMap Dat) -> String
             showStep inp = concat $ filter (not . null) $  (map (\(a,d) -> show a ++ " " ++ show d ++ "\n") $ I.toList inp) ++ ["."]

main :: IO()
main = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: vm binary conf\n"; 
  let file = args !! 0
  let conf = read (args !! 1) :: Dat
--  let verbose = (length args > 2)
  vm <- loadVMFromFile file
  let cont = case floor $ conf/1000.0 of
               1 -> task1Controller
               2 -> georgController
               9 -> getVTestController
               _ -> error "not implemented"
  let trace = runController (cont conf) vm
--  let vm' = runController2 (cont conf) vm
--  print vm'
  hPutStr stderr $ "Score: " ++ show (getOut 0 (traceOut (last trace)))
  writeFile ((show conf) ++ ".input") $ mkInputFile trace
  sequence_ ( map showFrame $ trace)
    where showFrame (Trace1 timeStep  inp out) = do
            let inmap = fromInp inp
                outmap = fromOutp out
            putStrLn $ "#time: " ++ (show timeStep)
            putStr "#inp:"
            putStrLn $ concat $ L.intersperse "#" $ map showConsoleOutput $ I.toAscList inmap
            putStrLn "#out:"
            putStr $ unlines $ map showConsoleOutput $ I.toAscList outmap
            putStrLn "."
            hFlush stdout
