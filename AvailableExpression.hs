module AvailableExpression where

import AnalysisTools
import MonotoneFramework
import Ast
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

availableExpression :: Program -> MFP Expression
availableExpression p@(stmts, _, _) =
  let addBottom = analyzerFor p Forward Must
      botm = aExp stmts
      addIota = addBottom botm
      addFunc = addIota Set.empty
      monoInstance = addFunc $ mkTransFunc killAE genAE botm
  in solveMFP monoInstance

killAE :: Statement -> Set Expression -> Set Expression
killAE (ExprStmt (AssignExpr _ (LVar x) _) _) init =
  Set.filter (Set.member x . fv) init
killAE _ _ = Set.empty

genAE :: Statement -> Set Expression
genAE (ExprStmt (AssignExpr _ (LVar x) expr) _) =
  Set.filter (not . Set.member x . fv) (arithSubExprs expr)
genAE (ExprStmt e _) = arithSubExprs e
genAE _ = Set.empty

killSets :: Program -> [(Label, [Expression])]
killSets (stmts,_, graph) =
  let bottm = aExp stmts
  in Map.toList $ Map.map (\a -> Set.toList $ killAE a bottm) graph

genSets :: Program -> [(Label, [Expression])]
genSets (_,_, graph) = Map.toList $ Map.map (Set.toList . genAE) graph
  
