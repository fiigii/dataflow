module MonotoneFramework where

import Ast
import Data.Set (Set, union, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, findWithDefault, (!))
import qualified Data.Map as Map
import AnalysisTools

data MonotoneFramework a = MonotoneInstance {
  completeLattice :: Lattice a,
  transferFunction :: TransferFunction a,
  flowF :: Set (Label, Label),
  extreLabE :: Set Label,
  iota :: Set a,
  bg :: BlockGraph
  }

data Lattice a = Lattice {
  leastUpperBound :: Set a -> Set a -> Set a,
  order :: Set a ->  Set a -> Bool,
  bottom :: Set a
  }

data MFP a = MFP {
  circle :: Map Label (Set a),
  dot :: Map Label (Set a)
  }
           deriving Show
  

data Direction = Forward | Backward
data Property = Must | May

type Kill a = Statement -> Set a -> Set a
type Gen a = Statement -> Set a
type TransferFunction a = Statement -> Set a -> Set a

analyzerFor :: Ord a => Statement -> BlockGraph -> Direction ->  Property -> Set a -> Set a -> (Statement -> Set a -> Set a) -> MonotoneFramework a
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
  bg = g
  }

mkTransFunc :: Ord a => Kill a -> Gen a -> Set a -> TransferFunction a
mkTransFunc kill gen bottm stmt set = 
  (set `difference` kill stmt bottm) `union` gen stmt

solveMFP :: Ord a => MonotoneFramework a -> MFP a
solveMFP monotone =
  let iterateSolver [] analy = analy  
      iterateSolver ((l, l') : ws) analy =
        if not $ lessThan new old
        then let newWorkList = allFlowStart l' flw ++ ws
                 newAnalysis = Map.insert l' (new `joion` old) analy
             in iterateSolver newWorkList newAnalysis 
        else iterateSolver ws analy
        where lStmt = g ! l
              new = func lStmt $ findWithDefault bottm l analy
              old = findWithDefault bottm l' analy
      resultAnalysis = iterateSolver workList initAnalysis
  in MFP {
    circle = resultAnalysis,
    dot = transMany resultAnalysis
    }
  where  flw = flowF monotone
         workList = Set.toList flw
         extremalLables = Set.toList $ extreLabE monotone
         initAnalysis = Map.fromList $ zip extremalLables $ repeat (iota monotone)
         func = transferFunction monotone
         g = bg monotone
         lessThan = order $ completeLattice monotone
         bottm = bottom $ completeLattice monotone
         joion = leastUpperBound $ completeLattice monotone
         transMany = Map.mapWithKey (\k a -> func (g ! k) a)
