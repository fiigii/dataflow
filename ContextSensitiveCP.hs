module ContextSensitiveCP where

import qualified Data.Set as Set
import Data.Map ((!), fromList, insert, adjust,delete)
import qualified Data.Map as Map
import ContextSensitiveMF
import AnalysisTools
import Ast

cpA :: Expression -> State -> ConstOrVar
cpA (Var v) sigma = sigma ! v
cpA (IntLit n) _ = Const n
cpA (InfixExpr op e1 e2) sigma
  | isArith op = (abstract op) (cpA e1 sigma)  (cpA e2 sigma)
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

csTransfer :: Int -> Statement -> Maybe CallStringState -> CallStringState -> CallStringState
csTransfer k (CallStmt _ _ (FunctionStmt _ es _ _) as Call lc) Nothing (omiga,csL) =
  let newEnv = take k $ lc : omiga
      oldState = csL ! omiga
      newCallStringSatet = Map.singleton newEnv (bindTo as es oldState)
  in (newEnv, newCallStringSatet)
csTransfer _ (CallStmt res _ (FunctionStmt _ _ body _) _ Return _) (Just (_, oldL)) (omiga, csL) =
  let (ReturnStmt (Just e) _) = last body
      upEnv = tail omiga
      sigma = csL ! omiga
      oldState = oldL ! upEnv
      resultState = insert res (cpA e sigma) oldState
      resultCallStringState = Map.singleton upEnv resultState
  in (upEnv, resultCallStringState)
csTransfer _ stmt Nothing (omiga, csL) = (omiga, adjust (transfer stmt) omiga csL)

resetArgs :: [String] -> State -> State
resetArgs [] sigma = sigma
resetArgs (s:ss) sigma = resetArgs ss sigma `Map.union` adjust (const Bottom) s sigma 

make :: ConstOrVar -> Program -> State
make v program = fromList $
                 zip (Set.toList $ allFvP program) (repeat v)

constantInstance :: Program -> MonotoneFramework
constantInstance program@(_,_,g) = MonotoneInstance {
  completeLattice = Lattice {
      leastUpperBound = csUnion,
      order = csLess,
      bottom = ([], Map.empty)
      },
  transferFunction = csTransfer 1,
  flowF = entireFlow program,
  extreLabE = Set.singleton $ entireInitial program,
  iota = ([], Map.singleton [] $ make Bottom program),
  interF = interFlow program,
  bg = g
  }

constants :: Program -> MFP
constants = solveMFP . constantInstance
