module Lowering.ANF where

import Core.Common

import qualified Frontend.Syntax as S
import Frontend.Syntax hiding (Term(..), Context, emptyContext, bindLocal, bindGlobal)

import Control.Monad (zipWithM)
import Control.Monad.State


--
-- Lowering Datatypes
--

data Decl
  = Fn Name [(Name, Type)] Type Term
  deriving Show

data Term 
  = Let Name Type Expr Term 
  | LetIf Name Type Val Term Term Term
  | LetMatch Name Type Val [Term] Term
  | Assign Name Val
  | Ret Val
  deriving Show

data Expr
  = Arith AOp Val Val
  | Comp COp Val Val
  | Logic LOp Val Val
  | Call Name [Val]
  | Pair [Val]
  | Proj Int Val
  | Inj Int Val
  | Cast Int Val
  deriving Show

data Val
  = Var Name Type
  | NumLit Int
  | BoolLit Bool
  deriving Show


-- 
-- Lowering Entry
--

lower :: S.Context -> S.Term -> Sig -> (Decl, [Type])
lower ctx tm (Sig n ps r) = 
  let anfCtx = Context [] (ctxGlob ctx)
      fnCtx = foldl' (\c (p, ty) -> bindLocal (Var p ty) c) anfCtx ps
      (t, s) = lowerTerm fnCtx tm
  in (Fn n ps r t, concatMap (getTypesOf . snd) ps ++ s)

lowerTerm :: Context -> S.Term -> (Term, [Type])
lowerTerm ctx tm = 
  let (t, s) = runState (normalize ctx tm ret) (LoweringState 0 [])
  in (t, loweredTypes s)


-- 
-- Lowering Terms
-- 

normalize :: Context -> S.Term -> (Val -> ANF Term) -> ANF Term
normalize ctx tm k = case tm of
  S.Let _ _ v u -> 
    normalize ctx v $ \v' ->
      normalize (bindLocal v' ctx) u k

  S.Call f ts -> 
    normalizeList ctx ts $ \ts' ->
      letBind (getRetType f ctx) (Call f ts') k

  S.Var i -> k (getLocal i ctx)

  S.Pair ts -> 
    normalizeList ctx ts $ \ts' ->
      letBind (Prod $ map typeOf ts') (Pair ts') k

  S.Proj i t -> 
    normalize ctx t $ \t' ->
      letBind (projType t' i) (Proj i t') k

  S.Inj a i t -> 
    normalize ctx t $ \t' ->
      letBind a (Inj i t') k

  S.Match a v bs ->
    normalize ctx v $ \v' -> do
      registerType a
      n <- fresh

      let doBranch :: (Name, S.Term) -> Int -> ANF Term
          doBranch (b, t) i = Let b (injType v' i) (Cast i v') <$> normalize (bindLocal (Var b $ injType v' i) ctx) t (pure . Assign n)

      bs' <- zipWithM doBranch bs [0..]

      LetMatch n a v' bs' <$> k (Var n a)

  S.BoolLit b -> k (BoolLit b)

  S.If a v t u -> 
    normalize ctx v $ \v' -> do
      registerType a
      n <- fresh
      t' <- normalize ctx t (pure . Assign n)
      u' <- normalize ctx u (pure . Assign n)
      LetIf n a v' t' u' <$> k (Var n a)

  S.NumLit i -> k (NumLit i)

  S.Arith o t u -> 
    normalize ctx t $ \t' ->
      normalize ctx u $ \u' ->
        letBind Number (Arith o t' u') k

  S.Comp o t u -> 
    normalize ctx t $ \t' ->
      normalize ctx u $ \u' ->
        letBind Boolean (Comp o t' u') k

  S.Logic o t u -> 
    normalize ctx t $ \t' ->
      normalize ctx u $ \u' ->
        letBind Boolean (Logic o t' u') k


normalizeList :: Context -> [S.Term] -> ([Val] -> ANF Term) -> ANF Term
normalizeList _ [] k = k []
normalizeList ctx (t:ts) k = 
  normalizeList ctx ts $ \ts' -> 
    normalize ctx t $ \t' ->
      k (t' : ts')


-- 
-- ANF Monad
--

type ANF a = State LoweringState a

data LoweringState = LoweringState
  { loweredCount :: Int
  , loweredTypes :: [Type]
  }

fresh :: ANF Name
fresh = do
  i <- state $ \s -> (loweredCount s, s{ loweredCount = loweredCount s + 1 })
  pure ("t" ++ show i)

ret :: Val -> ANF Term
ret = pure . Ret

letBind :: Type -> Expr -> (Val -> ANF Term) -> ANF Term
letBind ty e k = do
  registerType ty
  n <- fresh
  Let n ty e <$> k (Var n ty)

registerType :: Type -> ANF ()
registerType t = modify $ \s -> s
  { loweredTypes = loweredTypes s ++ getTypesOf t 
  }

-- registerType t@(Prod ts) = do 
--   mapM_ registerType ts 
--   modify (\s -> s{ loweredTypes = loweredTypes s ++ [t] })
-- registerType t@(Sum ts) = do
--   mapM_ registerType ts
--   modify (\s -> s{ loweredTypes = loweredTypes s ++ [t] })
-- registerType _ = pure ()

getTypesOf :: Type -> [Type]
getTypesOf t@(Prod ts) = concatMap getTypesOf ts ++ [t]
getTypesOf t@(Sum ts)  = concatMap getTypesOf ts ++ [t]
getTypesOf _           = []


--
-- Context
--

data Context = Context
  { ctxBound  :: [Val]
  , ctxGlobal :: [Sig]
  }

emptyContext :: Context
emptyContext = Context [] []

bindLocal :: Val -> Context -> Context
bindLocal v ctx = ctx{ ctxBound = v : ctxBound ctx }

getLocal :: Ix -> Context -> Val
getLocal ix ctx = ctxBound ctx !! ix

bindGlobal :: Sig -> Context -> Context
bindGlobal s ctx = ctx{ ctxGlobal = s : ctxGlobal ctx }

getGlobal :: Name -> Context -> Sig
getGlobal n ctx = case findName n (ctxGlobal ctx) of
  Nothing -> error "Terms should be well scoped"
  Just sig -> sig

getRetType :: Name -> Context -> Type
getRetType n ctx = sigReturn (getGlobal n ctx)


-- 
-- Helpers
--

typeOf :: Val -> Type
typeOf (Var _ t)   = t
typeOf (NumLit _)  = Number
typeOf (BoolLit _) = Boolean

projType :: Val -> Int -> Type
projType (Var _ (Prod ts)) i = ts !! i
projType _                 _ = error "Terms should be well typed"
  
injType :: Val -> Int -> Type
injType (Var _ (Sum ts)) i = ts !! i
injType _                _ = error "Terms should be well typed"

