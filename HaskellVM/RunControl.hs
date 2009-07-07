module Main where

import Types
import Controller
import ControllerUtils
import Util
import SimpleController
import Load

import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy

import qualified Data.ByteString as B
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified Data.DList as DL
import Control.Monad
import System.IO
import System
import L2Stat


mkInputFile :: Trace -> String
mkInputFile trace = 
    let noCons = DL.toList $ DL.map ( \(Trace1 t (Inp m) _) -> (t, m)) trace
        imap = I.fromListWith (I.union) noCons
        max  = (maximum $ I.keys imap)
        allsteps = map (\t -> I.findWithDefault I.empty t imap) $ take max [0..]
    in unlines $ map showStep allsteps
       where showStep :: (I.IntMap Dat) -> String
             showStep inp = concat $ filter (not . null) $  (map (\(a,d) -> show a ++ " " ++ show d ++ "\n") $ I.toList inp) ++ ["."]

searchG_mu vm = do let t = I.filter pred (mem vm)
                   print "Testing Interesting Values"
                   print t
    where pred dat = (abs (dat - mu) <= 1)
                     || (abs (dat*m_e - mu)  <= 1)
                     || (abs (dat - m_e)  <= 1)

-- runController :: (Controller VM a)  -> VM -> Trace
runController controller vm = snd . fst $ (runState (runWriterT (controller)) vm)
                              -- snd $ (runWriter (runStateT (controller) vm))

-- runController2 :: (Controller VM a)  -> VM -> VM
runController2 controller vm = -- snd . fst $ (runState (runWriterT (controller)) vm)
                              (runWriter (runStateT (controller) vm))

main :: IO()
main = do
  args <- getArgs
  when (length args < 2) $ do fail "\nUsage: vm binary conf [withInp]\n"; 
  let file = args !! 0
  let conf = read (args !! 1) :: Dat
  let withInp = (length args > 2)
  vm <- loadVMFromFile file
  
  let cont = case floor $ conf/1000.0 of
               2 -> task2Controller
               3 -> task3Controller
               5 -> stupidController
               7 -> noopController 10000
               8 -> getVTestController2
               9 -> testHohmannController
               _ -> error "not implemented"
  let trace' = (runController (cont conf) start)
      trace = DL.toList trace'
--  let vm' = runController2 (cont conf) vm
--  print vm'
--  hPutStr stderr $ "#Score: " ++ show (getOut 0 (traceOut (last $ DL.toList trace)))
  when withInp $ writeFile ((show conf) ++ ".input") $ mkInputFile trace'
  sequence_ ( map showFrame $ trace)
--  hPutStr stderr $ "Score: " ++ show (getOut 0 (traceOut (last $ trace)))
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
