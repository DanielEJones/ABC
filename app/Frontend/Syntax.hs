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

  | Pair [Term]
  | Proj Int Term

  | Inj Type Int Term
  | Match Type Term [(Name, Term)]

  | ArrayLit Type [Term]
  | ArrayNew Type Term Term
  | ArrayGet Term Term
  | ArraySet Term Term Term
  | ArrayLen Term

  | Fold Type Term
  | UnFold Type Term

  | BoolLit Bool
  | If Type Term Term Term

  | NumLit Int
  | ByteLit Int

  | Arith AOp Term Term
  | Comp COp Term Term
  | Logic LOp Term Term
  deriving Show

data Type
  = Number
  | Byte
  | Boolean
  | Prod [Type]
  | Sum [Type]
  | Array Type
  | Fix Type
  | TypeVar Ix
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

  S.Pair{} -> err ctx (UnInferable "Pair must be checked.")

  S.Proj n t -> do
    (ttm, tty) <- infer ctx t
    case tty of
      Prod ts -> do 
        when (n >= length ts) (err ctx $ OutOfBounds (length ts - 1) n)
        pure (Proj n ttm, ts !! n)
      _       -> err ctx (CannotPerfomOn "projection" "non-product type")

  S.Inj{} -> err ctx (UnInferable "Inject must be checked.")

  S.Match{} -> err ctx (UnInferable "Match must be checked.")

  S.ArrayLit{} -> err ctx (UnInferable "Array literal must be checked.")

  S.ArrayNew{} -> err ctx (UnInferable "Array construction must be checked.")

  S.ArrayGet i t -> do
    itm <- check ctx i Number
    (ttm, tty) <- infer ctx t
    case tty of
      Array a -> pure (ArrayGet itm ttm, a)
      _ -> err ctx (CannotPerfomOn "indexing" "non-array type")

  S.ArraySet i t v -> do
    itm <- check ctx i Number
    (ttm, tty) <- infer ctx t
    case tty of
      Array a -> do
        vtm <- check ctx v a
        pure (ArraySet itm ttm vtm, Array a)
      _ -> err ctx (CannotPerfomOn "indexing" "non-array type")

  S.ArrayLen t -> do
    (ttm, tty) <- infer ctx t
    case tty of
      Array{} -> pure (ArrayLen ttm, Number)
      _ -> err ctx (CannotPerfomOn "length" "non-array type")

  S.Fold{} -> err ctx (UnInferable "fold must be checked")

  S.UnFold t -> do
    (ttm, tty) <- infer ctx t
    case tty of
      Fix{} -> let a = unfoldType tty 
               in pure (UnFold a ttm, a)
      _ -> err ctx (CannotPerfomOn "unfold" "non-recursive type")

  S.BoolLit b -> pure (BoolLit b, Boolean)

  S.If{} -> err ctx (UnInferable "If must be checked.")

  S.NumLit i -> if i < 256 
    then pure (ByteLit i, Byte)
    else pure (NumLit i, Number)

  S.CharLit ch -> pure (ByteLit $ fromEnum ch, Byte)

  S.StringLit str -> pure (ArrayLit (Array Byte) $ map (ByteLit . fromEnum) str, Array Byte)

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

  (S.Pair ts, Prod as) -> do
    when (length ts /= length as) (err ctx $ ArgumentMismatch "pair" (length as) (length ts))
    cts <- zipWithM (check ctx) ts as
    pure (Pair cts)

  (S.Pair{}, a) -> err ctx (TypeMismatch (Prod []) a)

  (S.Inj n t, a@(Sum as)) -> do
    when (n >= length as) (err ctx $ OutOfBounds (length as - 1) n)
    ct <- check ctx t (as !! n)
    pure (Inj a n ct)

  (S.Inj{}, a) -> err ctx (TypeMismatch (Sum []) a)

  (S.Match t bs, a) -> do
    (ttm, tty) <- infer ctx t
    case tty of
      Sum ts -> do
        when (length bs /= length ts) (err ctx $ ArgumentMismatch "match" (length ts) (length bs))
        let checkBranch (n, u) bty = pair n <$> check (bindLocal n bty ctx) u a
        cbs <- zipWithM checkBranch bs ts
        pure (Match a ttm cbs)
      _ -> err ctx (CannotPerfomOn "match" "non-sum type")

  (S.ArrayLit ts, Array a) -> do
    ArrayLit (Array a) <$> mapM (\t -> check ctx t a) ts

  (S.ArrayLit{}, a) -> err ctx (TypeMismatch (Array a) a)

  (S.ArrayNew l t, Array a) -> do
    ltm <- check ctx l Number
    ttm <- check ctx t a
    pure (ArrayNew (Array a) ltm ttm)

  (S.ArrayNew{}, a) -> err ctx (TypeMismatch (Array a) a)

  (S.Fold t, Fix a) -> do
    ctm <- check ctx t (unfoldType $ Fix a)
    pure (Fold (Fix a) ctm)

  (S.Fold{}, a) -> err ctx (TypeMismatch (Fix a) a)

  (S.If v t u, a) -> do
    vtm <- check ctx v Boolean
    ttm <- check ctx t a
    utm <- check ctx u a
    pure (If a vtm ttm utm)

  (S.NumLit b, Byte) 
    | 0 <= b && b <= 255 -> pure (ByteLit b)
    | otherwise -> err ctx (NumLitOutOfRange 0 255 b)

  (S.NumLit i, Number) -> pure (NumLit i)

  (t, a) -> do
    (ttm, tty) <- infer ctx t
    if tty /= a && not (a == Number && tty == Byte)
      then err ctx (TypeMismatch tty a)
      else pure ttm

checkType :: Context -> S.Type -> Elab Type
checkType ctx ty = case ty of
  S.TyLoc l a -> checkType (withLoc l ctx) a
  S.Number -> pure Number
  S.Byte -> pure Byte
  S.Boolean -> pure Boolean
  S.Prod ts -> Prod <$> mapM (checkType ctx) ts
  S.Sum ts -> Sum <$> mapM (checkType ctx) ts
  S.Array t -> Array <$> checkType ctx t
  S.Fix n t -> Fix <$> checkType (bindType n ctx) t
  S.TypeVar n -> case findType n ctx of
    Just ix -> pure (TypeVar ix)
    Nothing -> err ctx (UnboundTypeVar n)

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
  | UnboundTypeVar Name
  | TypeMismatch Type Type
  | ArgumentMismatch Name Int Int
  | CannotPerfomOn String String
  | OutOfBounds Int Int
  | NumLitOutOfRange Int Int Int
  deriving Show


--
-- Contexts
--

data Context = Context 
  { ctxLoc   :: Loc
  , ctxVars  :: [(Name, Type)]
  , ctxGlob  :: [Sig]
  , ctxTypes :: [Name]
  }

emptyContext :: Loc -> Context
emptyContext loc = Context loc [] [] []

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

bindType :: Name -> Context -> Context
bindType n ctx = ctx{ ctxTypes = n : ctxTypes ctx }

findType :: Name -> Context -> Maybe Ix
findType n ctx = go (ctxTypes ctx) 0
  where
    go :: [Name] -> Int -> Maybe Ix
    go (name:rest) ix
      | name == n = Just ix
      | otherwise = go rest (ix + 1)
    go [] _ = Nothing


--
-- Type substitution
--

-- | Turn `(fix n (t + n))` into `t + (fix n (t + n))`
unfoldType :: Type -> Type
unfoldType t@(Fix a) = substType 0 t a
unfoldType t           = t

-- | Replace all occurences of TVar ix with s in ty
substType :: Ix -> Type -> Type -> Type
substType ix s ty = case ty of
  Number -> Number
  Byte -> Byte
  Boolean -> Boolean
  Prod ts -> Prod (map (substType ix s) ts)
  Sum ts -> Sum (map (substType ix s) ts)
  Array t -> Array (substType ix s t)
  Fix t -> Fix (substType (ix + 1) (shiftType 0 1 s) t)
  TypeVar i 
    | i == ix -> s -- The binder we sub, replace it
    | i > ix -> TypeVar (i - 1) -- A binder after the one we removed, so dec
    | otherwise -> TypeVar i -- A binder before, stays

-- | Lift all the binders greater than cutoff by d
shiftType :: Ix -> Int -> Type -> Type
shiftType cutoff d ty = case ty of
  Number -> Number
  Byte -> Byte
  Boolean -> Boolean
  Prod ts -> Prod (map (shiftType cutoff d) ts)
  Sum ts -> Sum (map (shiftType cutoff d) ts)
  Array t -> Array (shiftType cutoff d t)
  Fix t -> Fix (shiftType (cutoff + 1) d t)
  TypeVar i
    | i >= cutoff -> TypeVar (i + d)
    | otherwise   -> TypeVar i

