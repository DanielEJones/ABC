module Generation.CodeGen where 

import Data.List (intercalate)

import Core.Common
import Frontend.Syntax hiding (Term(..))
import Lowering.ANF


emitSig :: Sig -> String
emitSig (Sig n ps r) = emitType r ++ " abc_" ++ mangleName n ++ "(" ++ emitParams ps ++ ");"

emitDecl :: Decl -> String
emitDecl (Fn n ps r t) = unlines
  [ emitType r ++ " abc_" ++ mangleName n ++ "(" ++ emitParams ps ++ ") {"
  , emitTerm t
  , "}"
  ]

emitParams :: [(Name, Type)] -> String
emitParams = intercalate ", " . map emitParam

emitParam :: (Name, Type) -> String
emitParam (n, t) = emitType t ++ " " ++ n

emitTerm :: Term -> String
emitTerm (Let n t e k)       = "  " ++ emitType t ++ " " ++ n ++ " = " ++ emitExpr e ++ ";\n" ++ emitTerm k
emitTerm (LetIf n t v l r k) = "  " ++ emitType t ++ " " ++ n ++ ";\n" ++ "  if (" ++ emitVal v ++ ") {\n" ++ emitTerm l ++ "\n  } else {\n" ++ emitTerm r ++ "\n  }\n" ++ emitTerm k
emitTerm (Assign n v)  = "  " ++ n ++ " = " ++ emitVal v ++ ";"
emitTerm (Ret v)       = "  return " ++ emitVal v ++ ";"

emitExpr :: Expr -> String
emitExpr (Arith o l r) = emitVal l ++ " " ++ emitAOp o ++ " " ++ emitVal r
emitExpr (Comp o l r)  = emitVal l ++ " " ++ emitCOp o ++ " " ++ emitVal r
emitExpr (Call f xs)   = "abc_" ++ mangleName f ++ "(" ++ intercalate ", " (map emitVal xs) ++ ")"

emitVal :: Val -> String
emitVal (Var n)         = n
emitVal (NumLit i)      = show i
emitVal (BoolLit True)  = "true"
emitVal (BoolLit False) = "false"

emitType :: Type -> String
emitType Number = "int"
emitType Boolean = "bool"

emitAOp :: AOp -> String
emitAOp Add = "+"
emitAOp Sub = "-"
emitAOp Mul = "*"
emitAOp Div = "/"

emitCOp :: COp -> String
emitCOp Eq  = "=="
emitCOp NEq = "!="
emitCOp Lt  = "<"
emitCOp LtE = "<="
emitCOp Gt  = ">"
emitCOp GtE = ">="


mangleName :: Name -> Name
mangleName = concatMap fix
  where
    fix ch
      | isValid ch = [ch]
      | otherwise = "_" ++ show (fromEnum ch) ++ "_"

    isValid c = 
         'a' <= c && c <= 'z' 
      || 'A' <= c && c <= 'Z'
      || '_' == c

