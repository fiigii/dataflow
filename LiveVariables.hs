module LiveVariables where

import AnalysisTools
import MonotoneFramework
import Ast
import Data.Set (Set)
import qualified Data.Set as Set

liveVariables :: Program -> MFP String
liveVariables p =
  let addBottomTop = analyzerFor p Backward May
      botm = Set.empty
      top = Set.empty
      addFunc = addBottomTop botm top
      monoInstance = addFunc $ mkTransFunc killLV genLV botm
  in solveMFP monoInstance

killLV :: Statement -> Set String -> Set String
killLV (ExprStmt (AssignExpr _ (LVar x) _) _) _ = Set.singleton x
killLV _ _ = Set.empty

genLV :: Statement -> Set String
genLV (ExprStmt (AssignExpr _ _ e) _) = fv e
genLV (ExprStmt e _) = fv e
genLV _ = Set.empty
