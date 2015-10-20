module VeryBusy where

import AnalysisTools
import MonotoneFramework
import Ast
import Data.Set (Set)
import qualified Data.Set as Set

veryBusyExpression :: Program -> MFP Expression
veryBusyExpression p@(stmts, _) =
  let addBottom = analyzerFor p Backward Must
      botm = aExp stmts
      addIota = addBottom botm
      addFunc = addIota Set.empty
      monoInstance = addFunc $ mkTransFunc killVB genVB botm
  in solveMFP monoInstance

killVB :: Statement -> Set Expression -> Set Expression
killVB (ExprStmt (AssignExpr _ (LVar x) _) _) init =
  Set.filter (Set.member x . fv) init
killVB _ _ = Set.empty

genVB :: Statement -> Set Expression
genVB (ExprStmt (AssignExpr _ (LVar _) expr) _) = arithSubExprs expr
genVB (ExprStmt e _) = arithSubExprs e
genVB _ = Set.empty
