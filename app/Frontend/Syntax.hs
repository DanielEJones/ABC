module Frontend.Syntax where

import Control.Monad (zipWithM, when)

import Core.Common
import qualified Frontend.Surface as S


--
-- Syntax Datatypes
--

data Term
  = Let Name Type Term Term
  | Call Name [Term]
  | Var Ix

  | BoolLit Bool
  | If Type Term Term Term

  | NumLit Int

  | Arith AOp Term Term
  | Comp COp Term Term
  | Logic LOp Term Term
  deriving Show

data Type
  = Number
  | Boolean
  deriving (Show, Eq)

data Sig = Sig
  { sigName   :: Name
  , sigParams :: [(Name, Type)]
  , sigReturn :: Type 
  }

instance HasName Sig where
  nameOf = sigName


-- 
-- Elaboration
--

infer :: Context -> S.Term -> Elab (Term, Type)
infer ctx tm = case tm of
  S.TmLoc l t -> infer (withLoc l ctx) t

  S.Let n t v u -> do
    ct <- checkType ctx t
    vtm <- check ctx v ct
    (utm, a) <- infer ctx u
    pure (Let n ct vtm utm, a)

  S.Call n ts -> case findGlobal n ctx of
    Nothing -> err ctx (UnboundFn n)
    Just (Sig _ ps r) -> do
      when (length ps /= length ts) $ err ctx (ArgumentMismatch n (length ps) (length ts))
      ps' <- zipWithM (check ctx) ts (map snd ps)
      pure (Call n ps', r)

  S.Var n -> case findLocal n ctx of
    Just (ix, a) -> pure (Var ix, a) 
    Nothing -> err ctx (UnboundVar n)

  S.BoolLit b -> pure (BoolLit b, Boolean)

  S.If v t u -> do
    vtm <- check ctx v Boolean
    (ttm, a) <- infer ctx t
    utm <- check ctx u a
    pure (If a vtm ttm utm, a)

  S.NumLit i -> pure (NumLit i, Number)

  S.Arith o t u -> do
    ttm <- check ctx t Number
    utm <- check ctx u Number
    pure (Arith o ttm utm, Number)

  S.Comp o t u -> do
    ttm <- check ctx t Number
    utm <- check ctx u Number
    pure (Comp o ttm utm, Boolean)

  S.Logic o t u -> do
    ttm <- check ctx t Boolean
    utm <- check ctx u Boolean
    pure (Logic o ttm utm, Boolean)

check :: Context -> S.Term -> Type -> Elab Term
check ctx tm ty = case (tm, ty) of
  (S.TmLoc l t, a) -> check (withLoc l ctx) t a

  (S.Let n t u v, a) -> do
    ct <- checkType ctx t
    utm <- check ctx u ct
    vtm <- check (bindLocal n ct ctx) v a
    pure (Let n ct utm vtm)

  (S.If v t u, a) -> do
    vtm <- check ctx v Boolean
    ttm <- check ctx t a
    utm <- check ctx u a
    pure (If a vtm ttm utm)

  (t, a) -> do
    (ttm, tty) <- infer ctx t
    if tty /= a
      then err ctx (TypeMismatch tty a)
      else pure ttm

checkType :: Context -> S.Type -> Elab Type
checkType ctx ty = case ty of
  S.TyLoc l a -> checkType (withLoc l ctx) a
  S.Number -> pure Number
  S.Boolean -> pure Boolean

checkSig :: Context -> S.Decl -> Elab Sig
checkSig ctx decl = case decl of
  S.DLoc l d -> checkSig (withLoc l ctx) d

  S.FnDef n ps r _ -> 
    Sig n <$> mapM (\(name, ty) -> pair name <$> checkType ctx ty) ps 
          <*> checkType ctx r

checkDecl :: Context -> S.Term -> Sig -> Elab Term
checkDecl ctx tm (Sig _ ps r) = do
  let fnCtx = foldl' (\c (n, t) -> bindLocal n t c) ctx ps
  check fnCtx tm r


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
  | UnboundFn Name
  | TypeMismatch Type Type
  | ArgumentMismatch Name Int Int
  deriving Show


--
-- Contexts
--

data Context = Context 
  { ctxLoc  :: Loc
  , ctxVars :: [(Name, Type)]
  , ctxGlob :: [Sig]
  }

emptyContext :: Loc -> Context
emptyContext loc = Context loc [] []

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

bindGlobal :: Sig -> Context -> Context
bindGlobal s ctx = ctx{ ctxGlob = s : ctxGlob ctx }

findGlobal :: Name -> Context -> Maybe Sig
findGlobal n ctx = findName n (ctxGlob ctx)

