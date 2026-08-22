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
  , emitTerm 1 t
  , "}"
  ]

emitParams :: [(Name, Type)] -> String
emitParams = intercalate ", " . map emitParam

emitParam :: (Name, Type) -> String
emitParam (n, t) = emitType t ++ " " ++ n

emitStructFields :: [Val] -> String
emitStructFields xs = intercalate ", " (zipWith emitStructField xs [0..])

emitStructField :: Val -> Int -> String
emitStructField v i = "._" ++ show i ++ " = " ++ emitVal v

emitTerm :: Int -> Term -> String
emitTerm depth (Let n t e k) = 
     indent depth ++ emitType t ++ " " ++ n ++ " = " ++ emitExpr e ++ ";\n" 
  ++ emitTerm depth k
emitTerm depth (LetIf n t v l r k) = 
     indent depth ++ emitType t ++ " " ++ n ++ ";\n" 
  ++ indent depth ++ "if (" ++ emitVal v ++ ") {\n" 
  ++ emitTerm (depth + 1) l ++ "\n" 
  ++ indent depth ++ "} else {\n" 
  ++ emitTerm (depth + 1) r ++ "\n" 
  ++ indent depth ++ "}\n" 
  ++ emitTerm depth k
emitTerm depth (LetMatch n t v bs k) = 
     indent depth ++ emitType t ++ " " ++ n ++ ";\n"
  ++ indent depth ++ "switch (" ++ emitVal v ++ ".tag" ++ ") {\n"
  ++ (concat $ zipWith (\i b -> 
           indent (depth + 1) ++ "case " ++ show i ++ ": {\n"
        ++ emitTerm (depth + 2) b ++ "\n"
        ++ indent (depth + 1) ++ "} break;\n"
     ) [0 :: Int ..] bs)
  ++ indent depth ++ "}\n"
  ++ emitTerm depth k
emitTerm depth (Assign n v) = indent depth ++ n ++ " = " ++ emitVal v ++ ";"
emitTerm depth (Ret v) = indent depth ++ "return " ++ emitVal v ++ ";"

emitExpr :: Expr -> String
emitExpr (Arith o l r) = emitVal l ++ " " ++ emitAOp o ++ " " ++ emitVal r
emitExpr (Comp o l r)  = emitVal l ++ " " ++ emitCOp o ++ " " ++ emitVal r
emitExpr (Logic o l r) = emitVal l ++ " " ++ emitLOp o ++ " " ++ emitVal r
emitExpr (Call f xs)   = "abc_" ++ mangleName f ++ "(" ++ intercalate ", " (map emitVal xs) ++ ")"
emitExpr (Pair xs)     = "{ " ++ emitStructFields xs ++ " }"
emitExpr (Inj i v)     = "{ .tag = " ++ show i ++ ", .val._" ++ show i ++ " = " ++ emitVal v ++ " }"
emitExpr (Cast i v)    = "(" ++ emitVal v ++ ").val._" ++ show i
emitExpr (Proj i v)    = emitVal v ++ "._" ++ show i

emitVal :: Val -> String
emitVal (Var n _)       = n
emitVal (NumLit i)      = show i
emitVal (BoolLit True)  = "true"
emitVal (BoolLit False) = "false"

emitType :: Type -> String
emitType Number = "int"
emitType Boolean = "bool"
emitType (Prod ts) = "prod_" ++ intercalate "_" (map emitType ts) ++ "_end"
emitType (Sum ts) = "sum_" ++ intercalate "_" (map emitType ts) ++ "_end"

emitTypeDef :: Type -> String
emitTypeDef t@(Prod ts) = "typedef struct { " ++ emitStructFieldTypes ts ++ " } " ++ emitType t ++ ";"
emitTypeDef t@(Sum ts)  = "typedef struct { int tag; union { " ++ emitUnionVariantTypes ts ++ " } val; } " ++ emitType t ++ ";"
emitTypeDef _           = error "Not a user defined type"

emitStructFieldTypes :: [Type] -> String
emitStructFieldTypes ts = intercalate " " (zipWith emitStructFieldType ts [0..])

emitStructFieldType :: Type -> Int -> String
emitStructFieldType t i = emitType t ++ " _" ++ show i ++ ";" 

emitUnionVariantTypes :: [Type] -> String
emitUnionVariantTypes ts = intercalate " " (zipWith emitUnionVariantType ts [0..])

emitUnionVariantType :: Type -> Int -> String
emitUnionVariantType t i = emitType t ++ " _" ++ show i ++ ";"

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

emitLOp :: LOp -> String
emitLOp And = "&&"
emitLOp Or  = "||"


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

indent :: Int -> String
indent 0     = ""
indent depth = "  " ++ indent (depth - 1)

