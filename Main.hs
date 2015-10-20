module Main where

import System.IO
import System.Environment
import Parser
import AvailableExpression
import VeryBusy
import ReachingDefinition
import LiveVariables
import MonotoneFramework
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = do f : _ <- getArgs
          withFile ("test/" ++ f) ReadMode
            (\handle -> do source <- hGetContents handle
                           case parse source of
                            Right p ->
                              do putStr "\nAvailable Expression\n"
                                 let (MFP circle dot) = availableExpression p
                                     c = Map.toList circle
                                 putStr "Entry\n"
                                 pMFP c
                                 let d = Map.toList dot
                                 putStr "Exit\n"
                                 pMFP d
                                 putStr "\nVery Busy Expression\n"
                                 let (MFP cv dv) = veryBusyExpression p
                                     cv' = Map.toList cv
                                     dv' = Map.toList dv
                                 putStr "Entry\n"
                                 pMFP cv'
                                 putStr "Exits\n"
                                 pMFP dv'
                                 putStrLn "\nReaching Definition"
                                 let (MFP cr dr) = reachingDefinition p
                                     cr' = Map.toList cr
                                     dr' = Map.toList dr
                                 putStrLn "Extry"
                                 pMFP cr'
                                 putStrLn "Exit"
                                 pMFP dr'
                                 putStrLn "\nLive Variables"
                                 let (MFP cl dl) = liveVariables p
                                     cl' = Map.toList cl
                                     dl' = Map.toList dl
                                 putStrLn "Entry"
                                 pMFP cl'
                                 putStrLn "Exit"
                                 pMFP dl'
                            Left error -> putStr error)

pMFP :: Show a => [(Integer, Set.Set a)] -> IO ()
pMFP = mapM_ (\(l, s) -> putStr $ show l ++ " : " ++ show (Set.toList s) ++ "\n") 
