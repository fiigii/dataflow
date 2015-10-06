module MonotoneFramework where

import Ast
import Data.Set (Set, union, intersection)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (repeat)
import AnalysisTools

data MonotoneFramework a = MonotoneInstance {
  completeLattice :: Lattice a,
  transferFunction :: Statement -> (Set a),
  flowF :: Set (Label, Label),
  extreLabE :: Set Label,
  iota :: (Set a)
  graph :: BlockGraph
}

data Lattice a = Lattice {
  leastUpperBound :: Set a -> Set a -> Set a,
  order :: Set a ->  Set a -> Bool
  bottom :: Set a
}

data MFP a = MFP {
  circle :: Map Label (Set a),
  dot :: Map Label (Set a)
  }
  

data Direction = Forward | Backward
data Property = Must | May

analyzerFor :: Ord a => Statement -> BlockGraph -> Direction ->  Property -> (Set a) -> (Set a) -> (Statement -> Set a) -> MonotoneFramework a
analyzerFor s g dir m botm iotaValue f = MonotoneInstance {
  completeLattice = Lattice {
     leastUpperBound = case m of Must -> intersection; May -> union,
     order = case m of Must -> flip Set.isSubsetOf; May -> Set.isSubsetOf,
     bottom = botm
     },
  transferFunction = f,
  flowF = case dir of Forward -> flow s
                      Backward -> flowReverse $ flow s,
  extreLabE = case dir of Forward -> Set.singleton $ initial s
                          Backward -> final s,
  iota = iotaValue,
  graph = g
  }

solveMFP :: Ord a => MonotoneFramework a -> MFP a
solveMFP monotone =
  let workList = Set.toList $ flowF monotone
      extremalLables = extreLabE monotone
      actualExtremal = Set.toList $ Set.foldl'
                       (\acc (l1, l2) -> Set.insert l1 $ Set.insert l2 acc)
                       Set.empty $ flowF monotone
      analysis = Map.fromList $ zip actualExtremal $ repeat (iota monotone)
      func = transferFunction monotone
      g = graph monotone
      lessThan = order $ completeLattice monotone
      iterateSolver [] = MFP {circle = Map.empty, dot = Map.empty}
      iterateSolver ((l, l') : ws) =
        let lStmt = g !! l
            l'Stmt = g !! l'
  in MFP {circle = analysis, dot = analysis}
