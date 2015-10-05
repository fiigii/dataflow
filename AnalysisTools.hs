module AnalysisTools where

import Ast
import qualified Data.Set as Set
import Data.Set (union)
import Data.List (foldl')

initial :: Statement -> Label
initial (ExprStmt _ l) = l
initial (ReturnStmt _ l) = l
initial (EmptyStmt l) = l
initial (BlockStmt (s : _)) = initial s
initial (IfStmt _ _ _ l) = l
initial (IfSingleStmt _ _ l) = l
initial (WhileStmt _ _ l) = l
initial (FunctionStmt _ _ _ l) = l

final :: Statement -> Set.Set Label
final (ExprStmt _ l) = Set.singleton l
final (ReturnStmt _ l) = Set.singleton l
final (EmptyStmt l) = Set.singleton l
final (BlockStmt stmts) = final $ last stmts
final (IfStmt _ s1 s2 _) = final s1 `union` final s2
final (IfSingleStmt _ s l) = final s `union` Set.singleton l
final (WhileStmt _ _ l) = Set.singleton l
final (FunctionStmt _ _ body _) = final $ last body

blocks :: Statement -> Set.Set Statement
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
blocks f@(FunctionStmt _ _ body _) = Set.insert f $ blocks (BlockStmt body)

flow :: Statement -> Set.Set (Label, Label)
flow (ExprStmt _ _) = Set.empty
flow (ReturnStmt _ _) = Set.empty
flow (EmptyStmt _) = Set.empty
flow (BlockStmt [s]) = flow s
flow (BlockStmt (s : ss)) = flow s `union` flow (BlockStmt ss) `union` Set.fromList
                            [(l, initial (BlockStmt ss)) | l <- Set.toList $ final s]
flow (IfStmt _ s1 s2 l) = flow s1 `union` flow s2 `union`
                          Set.fromList [(l, initial s1), (l, initial s2)]
flow (IfSingleStmt _ s l) = flow s `union` Set.singleton (l, initial s)
flow (WhileStmt _ s l) = flow s `union` Set.singleton (l, initial s) `union`
                         Set.fromList [(l', l) | l' <- Set.toList $ final s]
flow (FunctionStmt _ _ body l) = flow (BlockStmt body) `union`
                                 Set.singleton (l, initial (BlockStmt body))

flowReverse :: Set.Set (Label, Label) -> Set.Set (Label, Label)
flowReverse = Set.map reverPair
  where reverPair (a, b) = (b, a)

fv :: Expression -> Set.Set String
fv (Var v) = Set.singleton v
fv (InfixExpr _ e1 e2) = fv e1 `union` fv e2
fv (CondExpr e1 e2 e3) = fv e1 `union` fv e2 `union` fv e3
fv (AssignExpr _ _ e) = fv e
fv (CallExpr f args) = fv f `union`
                       foldl' (\acc e -> fv e `union` acc)
                              Set.empty
                              args
fv _ = Set.empty

nontrivialSubExprs :: Expression -> Set.Set Expression
nontrivialSubExprs e@(InfixExpr _ e1 e2) = Set.singleton e `union`
                                           nontrivialSubExprs e1 `union`
                                           nontrivialSubExprs e2
nontrivialSubExprs e@(CondExpr e1 e2 e3) = Set.singleton e `union`
                                           nontrivialSubExprs e1 `union`
                                           nontrivialSubExprs e2 `union`
                                           nontrivialSubExprs e3
nontrivialSubExprs (AssignExpr _ _ e) = nontrivialSubExprs e
nontrivialSubExprs e@(CallExpr e1 es) =
  Set.singleton e `union`
  nontrivialSubExprs e1 `union`
  foldl' (\acc e' -> nontrivialSubExprs e' `union` acc) Set.empty es
nontrivialSubExprs _ = Set.empty
