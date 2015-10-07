module Parser (parse) where

import qualified Language.ECMAScript3.Parser as JsParser
import qualified Language.ECMAScript3.Syntax as Js
import Ast
import Control.Monad.RWS
import Control.Applicative
import qualified Data.Map.Strict as Map

type CFGGenerator = RWS String BlockGraph Label

parse :: String -> Either String Program
parse source = case JsParser.parseFromString source of
                Right ast -> let (stmts, graph) = run ast
                             in Right (BlockStmt stmts, graph)
                Left error -> Left $ show error
  where run p = evalRWS (mapM convertToAst $ Js.unJavaScript p) "" 1

convertToAst :: Js.Statement a -> CFGGenerator Statement
convertToAst (Js.BlockStmt _ ss) = do stmts <- mapM convertToAst ss
                                      return $ BlockStmt stmts
convertToAst (Js.ExprStmt _ expr) =
  do l <- freshLabel
     expr <- convertExpr expr
     let stmt = ExprStmt expr l
     addToMap l stmt
     return stmt
convertToAst (Js.IfStmt _ cond go els) =
  do l <- freshLabel
     expr <- convertExpr cond
     go' <- convertToAst go
     els' <- convertToAst els
     let stmt = IfStmt expr go' els' l
     addToMap l $ ExprStmt expr l
     return stmt
    
convertToAst (Js.IfSingleStmt _ cond go) =
  do l <- freshLabel
     expr <- convertExpr cond
     go' <- convertToAst go
     let stmt = IfSingleStmt expr go' l
     addToMap l $ ExprStmt expr l
     return stmt

convertToAst (Js.WhileStmt _ cond go) =
  do l <- freshLabel
     expr <- convertExpr cond
     go' <- convertToAst go
     let stmt = WhileStmt expr go' l
     addToMap l $ ExprStmt expr l
     return stmt
 
convertToAst (Js.ReturnStmt _ (Just expr)) =
  do l <- freshLabel
     expr' <- convertExpr expr
     let stmt = ReturnStmt (Just expr') l
     addToMap l stmt
     return stmt

convertToAst (Js.ReturnStmt _ Nothing) = do l <- freshLabel
                                            let stmt = ReturnStmt Nothing l
                                            addToMap l stmt
                                            return stmt
convertToAst (Js.FunctionStmt _ (Js.Id _ name) args body) =
  do l <- freshLabel
     body' <- mapM convertToAst body
     let stmt = FunctionStmt name (map (\(Js.Id _ arg) -> arg) args) body' l
     addToMap l stmt
     return stmt
convertToAst _ = EmptyStmt <$> freshLabel


convertExpr :: Js.Expression a -> CFGGenerator Expression
convertExpr (Js.StringLit _ str) = return $ StringLit str
convertExpr (Js.IntLit _ num) =  return $ IntLit num
convertExpr (Js.BoolLit _ bool) =  return $ BoolLit bool
convertExpr (Js.VarRef _ (Js.Id _ name)) =  return $ Var name
convertExpr (Js.InfixExpr _ op exp1 exp2) =
   InfixExpr (conertOp op) <$> (convertExpr exp1) <*> (convertExpr exp2)
convertExpr (Js.CondExpr _ cond exp1 exp2) =
  CondExpr <$> (convertExpr cond) <*> (convertExpr exp1) <*> (convertExpr exp2)
convertExpr (Js.AssignExpr _ op lv expr) =
  AssignExpr (conertAss op) <$> (convertLvalue lv) <*> (convertExpr expr)
convertExpr (Js.CallExpr _ func args) =
  CallExpr <$> (convertExpr func) <*> (mapM convertExpr args)
convertExpr (Js.FuncExpr _ name args body) =
  FuncExpr name' args' <$> mapM convertToAst body
  where name' = case name of Just (Js.Id _ n) -> Just n
                             Nothing -> Nothing
        args' = map (\(Js.Id _ n) -> n) args
convertExpr _ = return NullLit

freshLabel :: CFGGenerator Label
freshLabel = do i <- get
                put $ i + 1
                return i

addToMap :: Label -> Statement -> CFGGenerator ()
addToMap l s = tell $ Map.singleton l s


conertOp :: Js.InfixOp -> InfixOp
conertOp Js.OpLT = OpLT
conertOp Js.OpLEq = OpLEq
conertOp Js.OpGT = OpGT
conertOp Js.OpGEq = OpGEq
conertOp Js.OpIn = OpIn
conertOp Js.OpInstanceof = OpInstanceof
conertOp Js.OpEq = OpEq
conertOp Js.OpNEq = OpNEq
conertOp Js.OpStrictEq = OpStrictEq
conertOp Js.OpStrictNEq = OpStrictNEq
conertOp Js.OpLAnd = OpLAnd
conertOp Js.OpLOr = OpLOr
conertOp Js.OpMul = OpMul
conertOp Js.OpDiv = OpDiv
conertOp Js.OpMod = OpMod
conertOp Js.OpSub = OpSub
conertOp Js.OpLShift = OpLShift
conertOp Js.OpSpRShift = OpSpRShift
conertOp Js.OpZfRShift = OpZfRShift
conertOp Js.OpBAnd = OpBAnd
conertOp Js.OpBXor = OpBXor
conertOp Js.OpBOr = OpBOr
conertOp Js.OpAdd = OpAdd

conertAss :: Js.AssignOp -> AssignOp
conertAss Js.OpAssign = OpAssign
conertAss Js.OpAssignAdd = OpAssignAdd
conertAss Js.OpAssignSub = OpAssignSub
conertAss Js.OpAssignMul = OpAssignMul
conertAss Js.OpAssignDiv = OpAssignDiv
conertAss Js.OpAssignMod = OpAssignMod
conertAss Js.OpAssignLShift = OpAssignLShift
conertAss Js.OpAssignSpRShift = OpAssignSpRShift
conertAss Js.OpAssignZfRShift = OpAssignZfRShift
conertAss Js.OpAssignBAnd = OpAssignBAnd
conertAss Js.OpAssignBXor = OpAssignBXor
conertAss Js.OpAssignBOr = OpAssignBOr


convertLvalue (Js.LVar _ str) = return $ LVar str
convertLvalue (Js.LDot _ expr str) =
  do e <- convertExpr expr
     return $ LDot e str
convertLvalue (Js.LBracket _ exp1 exp2) =
  LBracket <$> (convertExpr exp1) <*> (convertExpr exp2)


