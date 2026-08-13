module Generation.CodeGen where 

import Core.Common (Op(..))
import Frontend.Syntax (Type(..))
import Lowering.ANF


emit :: Term -> String
emit t = unlines
  [ "#include <stdio.h>"
  , "int main() {"
  , emitTerm t
  , "}"
  ]

emitTerm :: Term -> String
emitTerm (Let n t e k) = "  " ++ emitType t ++ " " ++ n ++ " = " ++ emitExpr e ++ ";\n" ++ emitTerm k
emitTerm (Ret v)       = "  printf(\"%d\\n\", " ++ emitVal v ++ ");\n"

emitExpr :: Expr -> String
emitExpr (Arith o l r) = emitVal l ++ " " ++ emitOp o ++ " " ++ emitVal r

emitVal :: Val -> String
emitVal (Var n)    = n
emitVal (NumLit i) = show i

emitType :: Type -> String
emitType Number = "int"

emitOp :: Op -> String
emitOp Add = "+"
emitOp Sub = "-"
emitOp Mul = "*"
emitOp Div = "/"

