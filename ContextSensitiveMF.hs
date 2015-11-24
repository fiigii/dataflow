module ContextSensitiveMF where

import Ast
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, findWithDefault, (!), foldlWithKey') 
import qualified Data.Map as Map
import AnalysisTools

import Debug.Trace

data ConstOrVar = Const Int
                | Top
                | Bottom
                deriving (Eq, Show)
  
instance Ord ConstOrVar where
  x <= y | x == y = True
  (Const x) <= (Const y) = x == y
  _ <= Top = True
  _ <= Bottom = False
  Top <= _ = False
  Bottom <= _ = True

combin :: ConstOrVar -> ConstOrVar -> ConstOrVar
combin a@(Const x) (Const y) = if x == y
                               then a
                               else Top
combin Top _ = Top
combin _ Top = Top
combin Bottom a = a
combin a Bottom = a

type State = Map String ConstOrVar

makeEmpty :: State -> State
makeEmpty s = Map.map (const Bottom) s

less :: State -> State -> Bool
less s1 s2 = foldlWithKey' (\acc k v -> acc && v <= s2 ! k) True s1

union :: State -> State -> State
union s1 s2 = Map.mapWithKey (\k v -> combin v $ s2 ! k) s1

type CallStringState = ([Label], Map [Label] State)

csLess :: CallStringState -> CallStringState -> Bool
csLess (_, css1) (_, css2) = foldlWithKey' (\acc k v -> acc && less v (css2 ! k)) True css1

csUnion :: CallStringState -> CallStringState -> CallStringState
csUnion (e, css1) (_, css2) = (e,Map.unionWith union css1 css2)

data MonotoneFramework = MonotoneInstance {
  completeLattice :: Lattice,
  transferFunction :: Statement -> Maybe CallStringState -> CallStringState -> CallStringState,
  flowF :: Set FlowElement,
  extreLabE :: Set Label,
  iota :: CallStringState,
  interF :: Set InterFlowElement,
  bg :: BlockGraph
  }

data Lattice = Lattice {
  leastUpperBound :: CallStringState -> CallStringState -> CallStringState,
  order :: CallStringState ->  CallStringState -> Bool,
  bottom :: CallStringState
  }

data MFP = MFP {
  circle :: Map Label CallStringState,
  dot :: Map Label CallStringState
  }
           deriving Show

solveMFP :: MonotoneFramework -> MFP
solveMFP monotone =
  let iterateSolver [] analy = analy  
      iterateSolver ((Intra l l') : ws) analy =
        if new /= old
        then let newWorkList = if isReturn lStmt'
                               then allInterFlowStart l' interFlw (head current) ++ ws
                               else allFlowStart l' flw ++ ws
                 newAnalysis = Map.insert l' (new `joion` old) analy
             in iterateSolver newWorkList newAnalysis
        else iterateSolver ws analy
        where lStmt = g ! l
              lStmt' = g ! l'
              new@(current, _) = if isReturnBack lStmt
                                 then let oldLable = findCallSite l interFlw
                                          toRestore = findWithDefault bottm oldLable analy
                                          res = findWithDefault bottm l analy
                                      in  func lStmt (Just toRestore) res
                                 else func lStmt Nothing $ findWithDefault bottm l analy
              --new@(current, _) = func lStmt Nothing $ findWithDefault bottm l analy
              old = findWithDefault bottm l' analy
      iterateSolver ((Inter l l') : ws) analy =
        if new /= old
        then let newWorkList = allFlowStart l' flw ++ ws
                 newAnalysis = Map.insert l' (new `joion` old) analy
             in iterateSolver newWorkList newAnalysis
        else iterateSolver ws analy
        where lStmt = g ! l
              new =  func lStmt Nothing $ findWithDefault bottm l analy
              old = findWithDefault bottm l' analy
      resultAnalysis = iterateSolver workList initAnalysis
  in MFP {
    circle = resultAnalysis,
    dot = transMany resultAnalysis
    }

  where  flw = flowF monotone
         interFlw = interF monotone
         --workList = Set.toList flw
         workList = [x | l <- extremalLables ,x <- allFlowStart l flw]
         extremalLables = Set.toList $ extreLabE monotone
         initAnalysis = Map.fromList $ zip extremalLables $ repeat (iota monotone)
         func = transferFunction monotone
         g = bg monotone
         lessThan = order $ completeLattice monotone
         bottm = bottom $ completeLattice monotone
         joion = leastUpperBound $ completeLattice monotone
         --transMany = Map.mapWithKey (\k a -> func (g ! k) Nothing a)
         transMany analy =  Map.mapWithKey (\ k a -> let stmt = g ! k
                                                         oldLable = findCallSite k interFlw
                                                         toRestore = findWithDefault bottm oldLable analy
                                                     in if isReturnBack stmt
                                                        then func stmt (Just toRestore) a
                                                        else func stmt Nothing a ) analy
                                        

