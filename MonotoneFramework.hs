{-# LANGUAGE GADTs #-}
module MonotoneFramework where

import Ast
import Data.Set (Set, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, findWithDefault, (!))
import qualified Data.Map as Map
import AnalysisTools
import Lattice

data MonotoneFramework a where
  MonotoneInstance :: AbstractSet a => {
    bottom :: a,
    order :: a -> a -> Bool,
    leastUpperBound :: a -> a -> a,
    transferFunction :: Statement -> a -> a,
    flowF :: Set FlowElement,
    extreLabE :: Set Label,
    iota :: a,
    bg :: BlockGraph
  } -> MonotoneFramework a

data MFP a where
  MFP :: {
    circle :: Map Label a,
    dot :: Map Label a
    } -> MFP a

solveMFP :: (AbstractSet a, Eq a) => MonotoneFramework a -> MFP a
solveMFP monotone =
  let iterateSolver [] analy = analy  
      iterateSolver ((Intra l l') : ws) analy =
        if not $ new `lessThan` old
        then let newWorkList = allFlowStart l' flw ++ ws
                 newAnalysis = Map.insert l' (new `union` old) analy
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
         --workList = [x | l <- extremalLables ,x <- allFlowStart l flw]
         extremalLables = Set.toList $ extreLabE monotone
         initAnalysis = Map.fromList $ zip extremalLables $ repeat (iota monotone)
         func = transferFunction monotone
         g = bg monotone
         lessThan = order monotone
         bottm = bottom monotone
         transMany = Map.mapWithKey (\k a -> func (g ! k) a)
