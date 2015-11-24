module AnalysisTools where

import Ast
import qualified Data.Set as Set
import Data.Set (union, Set)
import Data.List (foldl')
import Data.Map ((!))

data FlowElement = Intra Label Label
                 | Inter Label Label
                   deriving (Eq, Ord)

type InterFlowElement = (Label, Label, Label, Label)

instance Show FlowElement where
  show (Intra l1 l2) = "(" ++ show l1 ++ ", " ++ show l2 ++ ")"
  show (Inter l1 l2) = "(" ++ show l1 ++ "; " ++ show l2 ++ ")"

initial :: Statement -> Label
initial (ExprStmt _ l) = l
initial (ReturnStmt _ l) = l
initial (BlockStmt []) = undefined
initial (BlockStmt (s : _)) = initial s
initial (IfStmt _ _ _ l) = l
initial (IfSingleStmt _ _ l) = l
initial (WhileStmt _ _ l) = l
initial (FunctionStmt _ _ _ ln) = ln
initial (CallStmt _ _ _ _ _ l) = l
initial (VarDeclStmt _ _ l) = l

final :: Statement -> Set Label
final (ExprStmt _ l) = Set.singleton l
final (ReturnStmt _ l) = Set.singleton l
final (BlockStmt stmts) = final $ last stmts
final (IfStmt _ s1 s2 _) = final s1 `union` final s2
final (IfSingleStmt _ s l) = final s `union` Set.singleton l
final (WhileStmt _ _ l) = Set.singleton l
final (FunctionStmt _ _ body _) = final $ last body
final (CallStmt _ _ _ _ _ l) = Set.singleton l
final (VarDeclStmt _ _ l) = Set.singleton l

blocks :: Statement -> Set Statement
blocks s@(ExprStmt _ _) = Set.singleton s
blocks s@(ReturnStmt _ _) = Set.singleton s
blocks (BlockStmt stmts) = foldl (\acc st -> acc `union` blocks st)
                                 Set.empty
                                 stmts
blocks (IfStmt cond s1 s2 l) = Set.singleton (ExprStmt cond l) `union`
                              blocks s1 `union` blocks s2
blocks (IfSingleStmt cond st l) = Set.insert (ExprStmt cond l) $ blocks st
blocks (WhileStmt cond st l) = Set.insert (ExprStmt cond l) $ blocks st
blocks f@(FunctionStmt _ _ body _ ) = Set.singleton f `union` blocks (BlockStmt body)
blocks c@(CallStmt{}) = Set.singleton c

flow :: Statement -> Set FlowElement
flow (BlockStmt [CallStmt _ _ (FunctionStmt _ _ body ln) _ Call lc, CallStmt _ _ (FunctionStmt{}) _ Return lr]) =
  let lx = initial $ last body
  in Set.fromList [Inter lc ln, Inter lx lr]
flow (BlockStmt [s]) = flow s
flow (BlockStmt (s : ss)) = flow s `union` flow (BlockStmt ss) `union` Set.fromList
                              [Intra l $  initial (BlockStmt ss) | l <- Set.toList $ final s]
flow (IfStmt _ s1 s2 l) = flow s1 `union` flow s2 `union`
                            Set.fromList [Intra l $ initial s1, Intra l $ initial s2]
flow (IfSingleStmt _ s l) = flow s `union` Set.singleton (Intra l $ initial s)
flow (WhileStmt _ s l) = flow s `union` Set.singleton (Intra l $ initial s) `union`
                           Set.fromList [Intra l' l | l' <- Set.toList $ final s]
flow (FunctionStmt _ _ body ln) = flow (BlockStmt body) `union`
                                       Set.singleton (Intra ln $ initial (BlockStmt body))
flow _ = Set.empty

flowReverse :: Set FlowElement -> Set FlowElement
flowReverse = Set.map reverPair
  where reverPair (Intra a b) = Intra b a
        reverPair (Inter a b) = Inter b a

interFlow :: Program -> Set InterFlowElement
interFlow (stmts, decls , _) =
  let allCallSites = callSites stmts `union` callSites (BlockStmt decls)
      funcLabels (FunctionStmt f _ body ln) = (ln, initial $ last body)
  in Set.map (\(f, func, lc, lr) -> let (ln, lx) = funcLabels func
                                    in (lc, ln, lx, lr)) allCallSites

entireFlow :: Program -> Set FlowElement
entireFlow (stmts, decls, _) = flow stmts `union` Set.unions declsFlow
  where declsFlow = map flow decls

entireInitial :: Program -> Label
entireInitial (stmts, _, _) = initial stmts

entireFinal :: Program -> Set Label
entireFinal (stmts, _, _) = final stmts

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
allFv (FunctionStmt _ args body _) = allFv (BlockStmt body) `union` Set.fromList args
allFv (CallStmt x _ _ es _ _) = Set.singleton x `union` foldl' (\acc e -> fv e `union` acc)
                                                        Set.empty
                                                        es
allFv (VarDeclStmt x (Just e) _) = Set.singleton x `union` fv e
allFv (VarDeclStmt x Nothing _) = Set.singleton x

allFvP :: Program -> Set String
allFvP (stmts, decls, _) =
  allFv stmts `union` allFv (BlockStmt decls)

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
aExp (CallStmt _ _ _ es _ _) =  foldl' (\acc e' -> arithSubExprs e' `union` acc) Set.empty es
aExp _ = Set.empty

allFlowStart :: Label -> Set FlowElement  -> [FlowElement]
allFlowStart l flw = Set.toList $ Set.filter test flw
  where test (Intra l' _) = l == l'
        test (Inter l' _) = l == l'

allInterFlowStart :: Label -> Set InterFlowElement -> Label -> [FlowElement]
allInterFlowStart l iflw env = foldl (\acc (lc, _, lx, lr) -> if l == lx && lc == env 
                                                               then Inter lx lr : acc
                                                               else acc) [] $ Set.toList iflw
findCallSite :: Label -> Set InterFlowElement -> Label
findCallSite l interflw = lc
  where (lc, _, _, _) = head $ Set.toList $ Set.filter (\(_, _, _, lr) -> lr == l) interflw

isReturn :: Statement -> Bool
isReturn ReturnStmt{} = True
isReturn _ = False

isReturnBack :: Statement -> Bool
isReturnBack (CallStmt _ _ _ _ Return _) = True
isReturnBack _ = False

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

callSites :: Statement -> Set (String, Statement, Label, Label)
callSites (BlockStmt []) = Set.empty
callSites (BlockStmt ((CallStmt _ f func _ Call lc): (CallStmt _ _ _ _ Return lr) : [])) = Set.singleton (f, func, lc, lr)
callSites (BlockStmt (s:ss)) = callSites s `union` callSites (BlockStmt ss)
callSites (IfStmt _ s1 s2 _) = callSites s1 `union` callSites s2
callSites (IfSingleStmt _ s _) = callSites s
callSites (WhileStmt _ s _) = callSites s
callSites (FunctionStmt _ _ body _) = callSites (BlockStmt body)
callSites _ = Set.empty

callOrReturn :: Statement -> CallSite
callOrReturn (CallStmt _ _ _ _ point _) = point
callOrReturn s = error $ show s ++ " is not a Call Statement"

stmtsGraph :: Statement -> [(Label, Statement)]
stmtsGraph (BlockStmt []) = []
stmtsGraph (BlockStmt (s:ss)) = stmtsGraph s ++ stmtsGraph (BlockStmt ss)
stmtsGraph s@(ExprStmt _ l) = [(l, s)]
stmtsGraph s@(CallStmt _ _ _ _ _ l) = [(l, s)]
stmtsGraph s@(IfStmt _ _ _ l) = [(l, s)]
stmtsGraph s@(IfSingleStmt _ _ l) = [(l, s)]
stmtsGraph s@(WhileStmt _ _ l) = [(l, s)]
stmtsGraph s@(ReturnStmt _ l) = [(l, s)]
stmtsGraph s@(FunctionStmt _ _ body l) = [(l, s)] ++
                                           stmtsGraph (BlockStmt body)
stmtsGraph _ = []

labels :: Statement -> Set Label
labels (BlockStmt []) = Set.empty
labels (BlockStmt (s:ss)) = labels s `union` labels (BlockStmt ss)
labels (ExprStmt _ l) = Set.singleton l
labels (CallStmt _ _ _ _ _ l) = Set.singleton l
labels (IfStmt _ s1 s2 l) = Set.singleton l `union` labels s1 `union` labels s2
labels (IfSingleStmt _ s l) = Set.singleton l `union` labels s
labels (WhileStmt _ s l) = Set.singleton l `union` labels s
labels (ReturnStmt _ l) = Set.singleton l
labels (FunctionStmt _ _ body ln) = Set.singleton ln `union` labels (BlockStmt body)
labels _ = Set.empty
