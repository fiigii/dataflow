Data Flow Analysis

This program is written in Haskell and depends on the Javascript library called language-ecmascript 0.17.

You can run the code with cabal and js file must be in "test/":
$ cabal run while.js
Preprocessing executable 'dataflow' for dataflow-0.1.0.0...
Running dataflow...

initial = 1

final = fromList [6]

blocks = fromList [ExprStmt (InfixExpr OpGT (Var "y") (IntLit 1)) 3,ExprStmt (AssignExpr OpAssign (LVar "y") (IntLit 0)) 6,ExprStmt (AssignExpr OpAssign (LVar "y") (Var "x")) 1,ExprStmt (AssignExpr OpAssign (LVar "y") (InfixExpr OpSub (Var "y") (IntLit 1))) 5,ExprStmt (AssignExpr OpAssign (LVar "z") (IntLit 1)) 2,ExprStmt (AssignExpr OpAssign (LVar "z") (InfixExpr OpMul (Var "z") (Var "y"))) 4]

flow = fromList [(1,2),(2,3),(3,4),(4,5),(5,3)]

$ cabal run while2.js
Preprocessing executable 'dataflow' for dataflow-0.1.0.0...
Running dataflow...

initial = 1

final = fromList [3]

blocks = fromList [ExprStmt (InfixExpr OpGT (Var "y") (InfixExpr OpAdd (Var "a") (Var "b"))) 3,ExprStmt (AssignExpr OpAssign (LVar "a") (InfixExpr OpAdd (Var "a") (IntLit 1))) 4,ExprStmt (AssignExpr OpAssign (LVar "x") (InfixExpr OpAdd (Var "a") (Var "b"))) 1,ExprStmt (AssignExpr OpAssign (LVar "x") (InfixExpr OpAdd (Var "a") (Var "b"))) 5,ExprStmt (AssignExpr OpAssign (LVar "y") (InfixExpr OpMul (Var "a") (Var "b"))) 2]

flow = fromList [(1,2),(2,3),(3,4),(4,5),(5,3)]