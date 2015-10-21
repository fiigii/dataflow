module AnalysisTools where

import Ast
import qualified Data.Set as Set
import Data.Set (union, difference, Set)
import Data.List (foldl')
import Data.Map ((!))

data FlowElement = Intra Label Label
                 | Inter Label Label
                   deriving (Eq, Ord)

instance Show FlowElement where
  show (Intra l1 l2) = "(" ++ show l1 ++ ", " ++ show l2 ++ ")"
  show (Inter l1 l2) = "(" ++ show l1 ++ "; " ++ show l2 ++ ")"

initial :: Statement -> Label
initial (ExprStmt _ l) = l
initial (ReturnStmt _ l) = l
initial (EmptyStmt l) = l
initial (BlockStmt (s : _)) = initial s
initial (IfStmt _ _ _ l) = l
initial (IfSingleStmt _ _ l) = l
initial (WhileStmt _ _ l) = l
initial (FunctionStmt _ _ _ ln _) = ln
initial (CallStmt _ _ _ lc _) = lc

final :: Statement -> Set Label
final (ExprStmt _ l) = Set.singleton l
final (ReturnStmt _ l) = Set.singleton l
final (EmptyStmt l) = Set.singleton l
final (BlockStmt stmts) = final $ last stmts
final (IfStmt _ s1 s2 _) = final s1 `union` final s2
final (IfSingleStmt _ s l) = final s `union` Set.singleton l
final (WhileStmt _ _ l) = Set.singleton l
final (FunctionStmt _ _ _ _ lx) = Set.singleton lx
final (CallStmt _ _ _ _ lr) = Set.singleton lr

blocks :: Statement -> Set Statement
blocks s@(ExprStmt _ _) = Set.singleton s
blocks s@(ReturnStmt _ _) = Set.singleton s
blocks s@(EmptyStmt _) = Set.singleton s
blocks (BlockStmt stmts) = foldl (\acc st -> acc `union` blocks st)
                                 Set.empty
                                 stmts
blocks (IfStmt cond s1 s2 l) = Set.singleton (ExprStmt cond l) `union`
                              blocks s1 `union` blocks s2
blocks (IfSingleStmt cond st l) = Set.insert (ExprStmt cond l) $ blocks st
blocks (WhileStmt cond st l) = Set.insert (ExprStmt cond l) $ blocks st
blocks f@(FunctionStmt _ _ body _ _) = Set.singleton f `union` blocks (BlockStmt body)
blocks c@(CallStmt{}) = Set.singleton c

flow :: Decls -> Statement -> Set FlowElement
flow _ (ExprStmt _ _) = Set.empty
flow _ (ReturnStmt _ _) = Set.empty
flow _ (EmptyStmt _) = Set.empty
flow d (BlockStmt [s]) = flow d s
flow d (BlockStmt (s : ss)) = flow d s `union` flow d (BlockStmt ss) `union` Set.fromList
                              [Intra l $  initial (BlockStmt ss) | l <- Set.toList $ final s]
flow d (IfStmt _ s1 s2 l) = flow d s1 `union` flow d s2 `union`
                            Set.fromList [Intra l $ initial s1, Intra l $ initial s2]
flow d (IfSingleStmt _ s l) = flow d s `union` Set.singleton (Intra l $ initial s)
flow d (WhileStmt _ s l) = flow d s `union` Set.singleton (Intra l $ initial s) `union`
                           Set.fromList [Intra l' l | l' <- Set.toList $ final s]
flow d (FunctionStmt _ _ body ln lx) = flow d (BlockStmt body) `union`
                                       Set.singleton (Intra ln $ initial (BlockStmt body)) `union`
                                       Set.map (`Intra` lx) (final (BlockStmt body))
flow d (CallStmt _ func _ lc lr) = let (ln, lx) = d ! func
                                   in Set.fromList [Inter lc ln, Inter lx lr]

flowReverse :: Set FlowElement -> Set FlowElement
flowReverse = Set.map reverPair
  where reverPair (Intra a b) = Intra b a
        reverPair (Inter a b) = Inter b a

interFlow :: Program -> Set (Label, Label, Label, Label)
interFlow (stmts, decls, _) =
  let allCallSites = callSites stmts
  in Set.map (\(f, lc, lr) -> let (ln, lx) = decls ! f
                              in (lc, ln, lx, lr)) allCallSites

fv :: Expression -> Set.Set String
fv (Var v) = Set.singleton v
fv (InfixExpr _ e1 e2) = fv e1 `union` fv e2
fv (CondExpr e1 e2 e3) = fv e1 `union` fv e2 `union` fv e3
fv (AssignExpr _ (LVar x) e) = fv e `union` Set.singleton x
fv (CallExpr f args) = fv f `union`
                       foldl' (\acc e -> fv e `union` acc)
                              Set.empty
                              args
fv _ = Set.empty

allFv :: Statement -> Set String
allFv (BlockStmt []) = Set.empty
allFv (BlockStmt (s:ss)) = allFv s `union` allFv (BlockStmt ss)
allFv (ExprStmt e _) = fv e
allFv (IfStmt cond th el _) = fv cond `union` allFv th `union` allFv el
allFv (IfSingleStmt cond th _) = fv cond `union` allFv th
allFv (WhileStmt cond body _) = fv cond `union` allFv body
allFv (ReturnStmt (Just e) _) = fv e
allFv (FunctionStmt _ args body _ _) = allFv (BlockStmt body) `difference` Set.fromList args
allFv _ = Set.empty

arithSubExprs :: Expression -> Set Expression
arithSubExprs e@(InfixExpr op e1 e2) | isArith op = Set.singleton e `union`
                                                    arithSubExprs e1 `union`
                                                    arithSubExprs e2
arithSubExprs (InfixExpr _ e1 e2) = arithSubExprs e1 `union` arithSubExprs e2
arithSubExprs (CondExpr e1 e2 e3) = arithSubExprs e1 `union`
                                    arithSubExprs e2 `union`
                                    arithSubExprs e3
arithSubExprs (AssignExpr _ _ e) = arithSubExprs e
arithSubExprs (CallExpr e1 es) =
  arithSubExprs e1 `union`
  foldl' (\acc e' -> arithSubExprs e' `union` acc) Set.empty es
arithSubExprs _ = Set.empty

isArith :: InfixOp -> Bool
isArith OpMul = True
isArith OpDiv = True
isArith OpMod = True
isArith OpSub = True
isArith OpAdd = True
isArith _ = False

aExp :: Statement -> Set Expression
aExp (BlockStmt []) = Set.empty
aExp (BlockStmt (s : ss)) =
  aExp s `union` aExp (BlockStmt ss)
aExp (ExprStmt e _) = arithSubExprs e
aExp (IfStmt cond s1 s2 _) =
  arithSubExprs cond `union` aExp s1 `union` aExp s2
aExp (IfSingleStmt cond s _) = arithSubExprs cond `union` aExp s
aExp (WhileStmt cond s _) = arithSubExprs cond `union` aExp s
aExp (ReturnStmt (Just e) _) = arithSubExprs e
aExp _ = Set.empty

allFlowStart :: Label -> Set FlowElement -> [FlowElement]
allFlowStart l flw = Set.toList $ Set.filter test flw
  where test (Intra l' _) = l == l'
        test (Inter l' _) = l == l'

allAssign :: Statement -> Set (String, Label)
allAssign (BlockStmt []) = Set.empty
allAssign (BlockStmt (s:ss)) =
  allAssign s `union` allAssign (BlockStmt ss)
allAssign (ExprStmt (AssignExpr _ (LVar x) _) l) = Set.singleton (x, l)
allAssign (IfStmt _ s1 s2 _) =
  allAssign s1 `union` allAssign s2
allAssign (IfSingleStmt _ s _) = allAssign s
allAssign (WhileStmt _ s _) = allAssign s
allAssign _ = Set.empty

callSites :: Statement -> Set (String, Label, Label)
callSites (BlockStmt []) = Set.empty
callSites (BlockStmt (s:ss)) = callSites s `union` callSites (BlockStmt ss)
callSites (CallStmt _ f _ lc lr) = Set.singleton (f, lc, lr)
callSites (IfStmt _ s1 s2 _) = callSites s1 `union` callSites s2
callSites (IfSingleStmt _ s _) = callSites s
callSites (WhileStmt _ s _) = callSites s
callSites (FunctionStmt _ _ body _ _) = callSites (BlockStmt body)
callSites _ = Set.empty

stmtsGraph :: Statement -> [(Label, Statement)]
stmtsGraph (BlockStmt []) = []
stmtsGraph (BlockStmt (s:ss)) = stmtsGraph s ++ stmtsGraph (BlockStmt ss)
stmtsGraph s@(ExprStmt _ l) = [(l, s)]
stmtsGraph s@(CallStmt _ _ _ l _) = [(l, s)]
stmtsGraph s@(IfStmt _ _ _ l) = [(l, s)]
stmtsGraph s@(IfSingleStmt _ _ l) = [(l, s)]
stmtsGraph s@(WhileStmt _ _ l) = [(l, s)]
stmtsGraph s@(ReturnStmt _ l) = [(l, s)]
stmtsGraph s@(FunctionStmt _ _ body l _) = [(l, s)] ++
                                           stmtsGraph (BlockStmt body)
stmtsGraph _ = []

labels :: Statement -> Set Label
labels (BlockStmt []) = Set.empty
labels (BlockStmt (s:ss)) = labels s `union` labels (BlockStmt ss)
labels (ExprStmt _ l) = Set.singleton l
labels (CallStmt _ _ _ lc lr) = Set.fromList [lc, lr]
labels (IfStmt _ s1 s2 l) = Set.singleton l `union` labels s1 `union` labels s2
labels (IfSingleStmt _ s l) = Set.singleton l `union` labels s
labels (WhileStmt _ s l) = Set.singleton l `union` labels s
labels (ReturnStmt _ l) = Set.singleton l
labels (FunctionStmt _ _ body ln lx) = Set.fromList [ln, lx] `union` labels (BlockStmt body)
labels _ = Set.empty
