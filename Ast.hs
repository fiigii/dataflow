module Ast where

import qualified Data.Map.Strict as Map

type Label = Integer
type Decls = Map.Map String (Label, Label)
type BlockGraph = Map.Map Label Statement
type Program = (Statement, Decls, BlockGraph)

data Statement = BlockStmt [Statement]
               | ExprStmt Expression Label
               | CallStmt String String [Expression] Label Label
               | IfStmt Expression Statement Statement Label
               | IfSingleStmt Expression Statement Label
               | WhileStmt Expression Statement Label
               | ReturnStmt (Maybe Expression) Label
               | FunctionStmt String [String] [Statement] Label Label
               | EmptyStmt Label
               deriving (Eq,Ord)

data Expression = StringLit String
                | IntLit Int
                | BoolLit Bool
                | Var String
                | InfixExpr InfixOp Expression Expression
                | CondExpr Expression Expression Expression
                | AssignExpr AssignOp LValue Expression
                | CallExpr Expression [Expression]
                | FuncExpr (Maybe String) [String] [Statement]
                | NullLit
                deriving (Eq,Ord)

data LValue
  = LVar String -- ^ variable reference, @foo@
  | LDot Expression String -- ^ @foo.bar@
  | LBracket Expression Expression -- ^ @foo[bar]@
  deriving (Eq, Ord)

instance Show LValue where
  show (LVar s) = s
  show (LDot e s) = show e ++ "." ++ s
  show (LBracket e1 e2) = show e1 ++ "[" ++ show e2 ++ "]"

instance Show Statement where
  show (BlockStmt stmts) = foldl (\acc s -> acc ++ show s) "" stmts
  show (ExprStmt e _) = show e
  show (IfStmt cond go els _) = "if( " ++ show cond ++ " ) {\n" ++
                                show go ++ "\n} else {\n" ++ show els ++
                                "\n}"
  show (IfSingleStmt cond go _) = "if( " ++ show cond ++ " ) {\n" ++
                                show go ++ "\n}"
  show (WhileStmt cond go _) =  "while( " ++ show cond ++ " ) {\n" ++
                                show go ++ "\n}"
  show (ReturnStmt (Just e) _) = "return " ++ show e
  show (ReturnStmt Nothing _) = "return"
  show (FunctionStmt f _ _ _ _) = "function " ++ f
  show (CallStmt res f args _ _) = res ++ " = " ++ f ++ "(" ++ show args ++ ")"
  show _ = ""

instance Show Expression where
  show (StringLit s) = s
  show (IntLit i) = show i
  show (BoolLit False) = "false"
  show (BoolLit True) = "true"
  show (Var s) = s
  show (InfixExpr op e1 e2) = show e1 ++ show op ++ show e2
  show (CondExpr cond go els) = show cond ++ " ? " ++ show go ++
                                " : " ++ show els
  show (AssignExpr op lv e) = show lv ++ show op ++ show e
  show (CallExpr f args) = show f ++ "(" ++
                           foldl (\acc arg -> acc ++ ", " ++ show arg)
                           "" args ++ ")"
  show (FuncExpr{}) = "<function>"
  show NullLit = "null"

-- | Infix operators: see spec 11.5-11.11
data InfixOp = OpLT -- ^ @<@
             | OpLEq -- ^ @<=@
             | OpGT -- ^ @>@
             | OpGEq -- ^ @>=@
             | OpIn -- ^ @in@
             | OpInstanceof -- ^ @instanceof@
             | OpEq -- ^ @==@
             | OpNEq -- ^ @!=@
             | OpStrictEq -- ^ @===@
             | OpStrictNEq -- ^ @!===@
             | OpLAnd -- ^ @&&@
             | OpLOr -- ^ @||@
             | OpMul -- ^ @*@
             | OpDiv -- ^ @/@
             | OpMod -- ^ @%@
             | OpSub -- ^ @-@
             | OpLShift -- ^ @<<@
             | OpSpRShift -- ^ @>>@
             | OpZfRShift -- ^ @>>>@
             | OpBAnd -- ^ @&@
             | OpBXor -- ^ @^@
             | OpBOr -- ^ @|@
             | OpAdd -- ^ @+@
             deriving (Eq,Ord)
instance Show InfixOp where
  show OpLT = " < "
  show OpLEq = " <= "
  show OpGT = " > "
  show OpGEq = " >= "
  show OpIn = " in "
  show OpInstanceof = " instanceof "
  show OpEq = " == "
  show OpNEq = " != "
  show OpStrictEq = " === "
  show OpStrictNEq = " !=== "
  show OpLAnd = " && "
  show OpLOr = " || "
  show OpMul = " * "
  show OpDiv = " / "
  show OpMod = " % "
  show OpSub = " - "
  show OpLShift = " << "
  show OpSpRShift = " >> "
  show OpZfRShift = " >>> "
  show OpBAnd = " & "
  show OpBXor = " ^ "
  show OpBOr = " | "
  show OpAdd = " + "
  

-- | Assignment operators: see spec 11.13
data AssignOp = OpAssign -- ^ simple assignment, @=@
              | OpAssignAdd -- ^ @+=@
              | OpAssignSub -- ^ @-=@
              | OpAssignMul -- ^ @*=@
              | OpAssignDiv -- ^ @/=@
              | OpAssignMod -- ^ @%=@
              | OpAssignLShift -- ^ @<<=@
              | OpAssignSpRShift -- ^ @>>=@
              | OpAssignZfRShift -- ^ @>>>=@
              | OpAssignBAnd -- ^ @&=@
              | OpAssignBXor -- ^ @^=@
              | OpAssignBOr -- ^ @|=@
              deriving (Eq, Ord)

instance Show AssignOp where
  show OpAssign = " = "
  show OpAssignAdd = " += "
  show OpAssignSub = " -= "
  show OpAssignMul = " *= "
  show OpAssignDiv = " /= "
  show OpAssignMod = " %= "
  show OpAssignLShift = " <<= "
  show OpAssignSpRShift = " >>= "
  show OpAssignZfRShift = " >>>= "
  show OpAssignBAnd = " &= "
  show OpAssignBXor = " ^= "
  show OpAssignBOr = " |= "
  
