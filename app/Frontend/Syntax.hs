module Frontend.Syntax where

import Core.Common
import qualified Frontend.Surface as S


--
-- Syntax Datatypes
--

data Term
  = Let Name Type Term Term
  | Var Ix

  | NumLit Int
  | Arith Op Term Term
  deriving Show

data Type
  = Number
  deriving (Show, Eq)


-- 
-- Elaboration
--

infer :: Context -> S.Term -> Elab (Term, Type)
infer ctx tm = case tm of
  S.TmLoc l t -> infer (withLoc l ctx) t

  S.Var n -> case findLocal n ctx of
    Just (ix, a) -> pure (Var ix, a) 
    Nothing -> err ctx (UnboundVar n)

  S.NumLit i -> pure (NumLit i, Number)

  S.Arith o t u -> do
    ttm <- check ctx t Number
    utm <- check ctx u Number
    pure (Arith o ttm utm, Number)

  S.Let{} -> err ctx (UnInferable "let binding")

check :: Context -> S.Term -> Type -> Elab Term
check ctx tm ty = case (tm, ty) of
  (S.TmLoc l t, a) -> check (withLoc l ctx) t a

  (S.Let n t u v, a) -> do
    ct <- checkType ctx t
    utm <- check ctx u ct
    vtm <- check (bindLocal n ct ctx) v a
    pure (Let n ct utm vtm)

  (t, a) -> do
    (ttm, tty) <- infer ctx t
    if tty /= a
      then err ctx (TypeMismatch tty a)
      else pure ttm

checkType :: Context -> S.Type -> Elab Type
checkType ctx ty = case ty of
  S.TyLoc l a -> checkType (withLoc l ctx) a
  S.Number -> pure Number


--
-- Elaboration Monad
--

type Elab a = Either Error a

data Error = Error
  { errorLoc :: Loc
  , errorMsg :: Error'
  }
  deriving Show

data Error' 
  = UnInferable String
  | UnboundVar Name
  | TypeMismatch Type Type
  deriving Show


--
-- Contexts
--

data Context = Context 
  { ctxLoc  :: Loc
  , ctxVars :: [(Name, Type)]
  }

emptyContext :: Loc -> Context
emptyContext loc = Context loc []

withLoc :: Loc -> Context -> Context
withLoc loc ctx = ctx{ ctxLoc = loc }

err :: Context -> Error' -> Elab a
err ctx = Left . Error (ctxLoc ctx)

bindLocal :: Name -> Type -> Context -> Context
bindLocal n t ctx = ctx{ ctxVars = (n, t) : ctxVars ctx }

findLocal :: Name -> Context -> Maybe (Ix, Type)
findLocal n ctx = go (ctxVars ctx) 0
  where
    go :: [(Name, Type)] -> Int -> Maybe (Ix, Type)
    go ((name, ty):rest) ix
      | name == n = Just (ix, ty)
      | otherwise = go rest (ix + 1)
    go [] _ = Nothing

