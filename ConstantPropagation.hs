{-# LANGUAGE TypeFamilies #-}
module ConstantPropagation where

import qualified Data.Set as Set
import Data.Map (Map, foldlWithKey', (!), fromList, insert, adjust)
import qualified Data.Map as Map
import MonotoneFramework
import AnalysisTools
import Ast

import Lattice

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

class Combinable a where
  combin :: a -> a -> a

instance Combinable ConstOrVar where
  combin a@(Const x) (Const y) = if x == y
                                 then a
                                 else Top
  combin Top _ = Top
  combin _ Top = Top
  combin Bottom a = a
  combin a Bottom = a

type State = Map String ConstOrVar

instance (Ord k, Ord v, Combinable v) => AbstractSet (Map k v) where
  type Element (Map k v) = (k, v)
  less s1 s2 = foldlWithKey' (\acc k v -> acc && v <= s2 ! k) True s1
  union s1 s2 = Map.mapWithKey (\k v -> combin v $ s2 ! k) s1
  singleton = uncurry Map.singleton
  difference = Map.difference

cpA :: Expression -> State -> ConstOrVar
cpA (Var v) sigma = sigma ! v
cpA (IntLit n) _ = Const n
cpA (InfixExpr op e1 e2) sigma
  | isArith op = abstract op (cpA e1 sigma)  (cpA e2 sigma)
cpA _ _ = Bottom                                    
                      
abstract :: InfixOp -> ConstOrVar -> ConstOrVar -> ConstOrVar
abstract op (Const n1) (Const n2) =
  Const $ case op of OpAdd -> n1 + n2
                     OpSub -> n1 - n2
                     OpMul -> n1 * n2
                     OpDiv -> n1 `div` n2
abstract _ x y | x == Bottom || y == Bottom = Bottom
abstract _ _ _ = Top

transfer :: Statement -> State -> State
transfer (ExprStmt (AssignExpr OpAssign (LVar x) e) _) sigma =
  insert x (cpA e sigma) sigma
transfer (VarDeclStmt x Nothing _) sigma = adjust (const Top) x sigma
transfer (VarDeclStmt x (Just e) _) sigma = adjust (const $ cpA e sigma) x sigma
transfer (CallStmt _ _ (FunctionStmt _ es _ _) as Call _) sigma = bindTo as es sigma
transfer (CallStmt res _ (FunctionStmt _ _ body _) _ Return _) sigma =
  let (ReturnStmt (Just e) _) = last body
  in adjust (const $ cpA e sigma) res sigma
transfer _ sigma = sigma

bindTo :: [Expression] -> [String] -> State -> State
bindTo as fs sigma = let newState = fromList $ zip fs $ map (\e ->  cpA e sigma) as
                     in newState `Map.union` sigma

make :: ConstOrVar -> Program -> State
make v program = fromList $
                 zip (Set.toList $ allFvP program) (repeat v)


constantInstance :: Program -> MonotoneFramework State
constantInstance program@(_,_,g) = MonotoneInstance {
  bottom = make Bottom program,
  transferFunction = transfer,
  flowF = entireFlow program,
  extreLabE = Set.singleton $ entireInitial program,
  iota = make Top program,
  bg = g
  }

constants :: Program -> MFP State
constants = solveMFP . constantInstance
