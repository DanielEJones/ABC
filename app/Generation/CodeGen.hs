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
emitTerm (Let n t e k) = "  " ++ emitType t ++ " " ++ n ++ " = " ++ emitExpr e ++ ";\n" ++ emitTerm k
emitTerm (Ret v)       = "  return " ++ emitVal v ++ ";"

emitExpr :: Expr -> String
emitExpr (Arith o l r) = emitVal l ++ " " ++ emitOp o ++ " " ++ emitVal r
emitExpr (Call f xs)   = "abc_" ++ mangleName f ++ "(" ++ intercalate ", " (map emitVal xs) ++ ")"

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


mangleName :: Name -> Name
mangleName = concatMap fix
  where
    fix ch
      | isAlpha ch || ch == '_' = [ch]
      | otherwise = "_" ++ show (fromEnum ch) ++ "_"
    isAlpha c = 
         'a' <= c && c <= 'z' 
      || 'A' <= c && c <= 'Z'

