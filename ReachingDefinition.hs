module ReachingDefinition where

import AnalysisTools
import MonotoneFramework
import Ast
import Data.Set (Set, union)
import qualified Data.Set as Set

data LabelOrUnknown = L Label | Unknown
                    deriving (Eq, Ord)
data Definition = Define String LabelOrUnknown
                deriving (Eq, Ord)

instance Show LabelOrUnknown where
  show (L l) = show l
  show Unknown = "?"

instance Show Definition where
  show (Define v l) = "(" ++ v ++ ", " ++ show l ++ ")"

reachingDefinition :: Program -> MFP Definition
reachingDefinition p@(stmts, _) =
  let addBottom = analyzerFor p Forward May
      botm = Set.empty
      addIota = addBottom botm
      top = Set.map (\s -> Define s Unknown) $ allFv stmts
      addFunc = addIota top
      allLabel = Set.map (\(x,l) -> Define x $ L l) $ allAssign stmts
      monoInstance = addFunc $ mkTransFunc killRD genRD allLabel
  in solveMFP monoInstance

killRD :: Statement -> Set Definition -> Set Definition
killRD (ExprStmt (AssignExpr _ (LVar x) _) _) init =
  Set.singleton (Define x Unknown) `union`
  Set.filter (\(Define v _) -> v == x) init
killRD _ _ = Set.empty

genRD :: Statement -> Set Definition
genRD (ExprStmt (AssignExpr _ (LVar x) _) l) =
  Set.singleton $ Define x (L l)
genRD _ = Set.empty
