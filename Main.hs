module Main where

import System.IO
import System.Environment
import Parser
import AvailableExpression
import MonotoneFramework
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = do f : _ <- getArgs
          withFile ("test/" ++ f) ReadMode
            (\handle -> do source <- hGetContents handle
                           case parse source of
                            Right p@(stmts, g) ->
                              do putStr "\nKill Set:\n"
                                 pMap $ killSets p
                                 putStr "\nGen Set:\n"
                                 pMap $ genSets p
                                 let (MFP circle dot) = availableExpression p
                                     c = Map.toList circle
                                 putStr "\nEntry\n"
                                 pMFP c
                                 let d = Map.toList dot
                                 putStr "\nExit\n"
                                 pMFP d
                             Left error -> print error)

pMFP = mapM_ (\(l, s) -> print $ show l ++ " : " ++ (show $ Set.toList s)) 
pMap = mapM_ (\(l, s) -> print $ show l ++ " : " ++ (show $ s)) 
