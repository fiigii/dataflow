{-# LANGUAGE TypeFamilies #-}
module Distributive where

import Ast
import Lattice
import MonotoneFramework
import AnalysisTools
import qualified Data.Set as Set
import Data.Set (Set)

data Direction = Forward | Backward
data Property = Must | May

instance Ord a => AbstractSet (Set a) where
  type Element (Set a) = a
  union = Set.union
  intersection = Set.intersection
  less = Set.isSubsetOf
  singleton = Set.singleton
  difference = Set.difference

analyzerFor :: AbstractSet a => Program -> Direction ->  Property -> a -> a -> (Statement -> a -> a) -> MonotoneFramework a
analyzerFor p@(s, decls, g) dir m botm iotaValue f = MonotoneInstance {
  bottom = botm,
  order = case m of Must -> flip less; May -> less,
  leastUpperBound = case m of Must -> intersection
                              May -> union,
  transferFunction = f,
  flowF = case dir of Forward -> entireFlow p
                      Backward -> flowReverse $ entireFlow p,
  extreLabE = case dir of Forward -> singleton $ entireInitial p
                          Backward -> entireFinal p,
  iota = iotaValue,
  bg = g
  }

mkTransFunc :: AbstractSet a => (Statement -> a -> a) -> (Statement -> a) -> a -> (Statement -> a -> a)
mkTransFunc kill gen bottm stmt set = 
  (set `difference` kill stmt bottm) `union` gen stmt
