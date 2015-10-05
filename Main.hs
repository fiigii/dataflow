module Main where

import System.IO
import System.Environment
import Parser
import AnalysisTools
import qualified Data.Map as Map

main :: IO ()
main = do f : _ <- getArgs
          withFile ("test/" ++ f) ReadMode
            (\handle -> do source <- hGetContents handle
                           case parse source of Right (ast, b) -> do putStr "\ninitial = "
                                                                     print $ initial ast
                                                                     putStr "\nfinal = "
                                                                     print $ final ast
                                                                     putStr "\nblocks = "
                                                                     print $ Map.elems b
                                                                     putStr "\nflow = "
                                                                     print $ flow ast
                                                                     print "program"
                                                                     print ast
                                                Left error -> print error)
           
