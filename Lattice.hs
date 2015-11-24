{-# LANGUAGE TypeFamilies#-}
module Lattice where

class AbstractSet set where
  type Element set :: *
  union :: set -> set -> set
  intersection :: set -> set -> set
  less :: set -> set -> Bool
  singleton :: Element set -> set
  difference :: set -> set -> set

