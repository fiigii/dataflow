module AvailableExpression where

import AnalysisTools
import MonotoneFramework
import Ast
import Distributive
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

availableExpression :: Program -> MFP (Set Expression)
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
