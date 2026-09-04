module Lowering.ANF where

import Core.Common

import qualified Frontend.Syntax as S
import Frontend.Syntax hiding (Term(..), Context, emptyContext, bindLocal, bindGlobal)

import Control.Monad (zipWithM, forM)
import Control.Monad.State

import Lowering.Core
import Lowering.MemoryManagement (insertMemoryOps)


-- 
-- Lowering Entry
--

lower :: S.Context -> S.Term -> Sig -> (Decl, [Type])
lower ctx tm (Sig n ps r) = (Fn n ps r loweredTm, paramTs ++ loweredTs)
  where
    params = map (uncurry Var) ps
    paramTs = concatMap (getTypesOfVal) params
    newContext = Context [] (ctxGlob ctx)
    fnContext = foldl' (flip bindLocal) newContext params
    (loweredTm, loweredTs) = lowerTerm fnContext tm

lowerTerm :: Context -> S.Term -> (Term, [Type])
lowerTerm ctx tm = extractTypes (runState doLower initialState)
  where
    doLower :: ANF Term
    doLower = do
      normalized <- normalize ctx tm ret
      insertMemoryOps (ctxBound ctx) normalized

    extractTypes :: (Term, LoweringState) -> (Term, [Type])
    extractTypes (t, s) = (t, loweredTypes s)

    initialState :: LoweringState
    initialState = LoweringState 0 [] [] 


-- 
-- Lowering Terms
-- 

normalize :: Context -> S.Term -> (Val -> ANF Term) -> ANF Term
normalize ctx tm k = case tm of
  S.Let pat _ v u ->
    normalize ctx v $ \v' ->
      normalizePattern ctx pat v' $ \ctx' ->
        normalize ctx' u k

  -- S.Let (PVar{}) _ v u -> 
  --   normalize ctx v $ \v' ->
  --     normalize (bindLocal v' ctx) u k

  -- S.Let (PTuple{}) t v u -> 
  --   normalize ctx v $ \v' -> do
  --     registerType t
  --     freshVals <- mapM (\ty -> pair <$> fresh <*> pure ty) (prodType t)
  --     let newCtx = foldr bindLocal ctx $ map (uncurry Var) freshVals
  --     LetMany freshVals v' <$> normalize newCtx u k

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

      -- let doBranch :: (Pattern, S.Term) -> Int -> ANF Term
      --     doBranch (b, t) i = do 
      --       let branchType = injType v' i
      --       case b of
      --         PVar bn -> Let bn branchType (Cast i v') 
      --                <$> normalize (bindLocal (Var bn branchType) ctx) t (pure . Assign n)
      --         PTuple ns -> do 
      --           bn <- fresh
      --           Let bn branchType (Cast i v')
      --                  <$> LetMany (zip ns $ prodType branchType) (Var bn branchType)
      --                  <$> normalize (bindManyLocals ns (prodType branchType) ctx) t (pure . Assign n)
      --     -- Let b (injType v' i) (Cast i v') <$> normalize (bindLocal (Var b $ injType v' i) ctx) t (pure . Assign n)

      bs' <- zipWithM (normalizeBranch ctx v' $ pure . Assign n) [0..] bs 
      LetMatch n a v' bs' <$> k (Var n a)

  S.ArrayLit a ts ->
    normalizeList ctx ts $ \ts' -> do
      registerType a
      n <- fresh
      LetArrayLit n a ts' <$> k (Var n a)

  S.ArrayNew a l d -> 
    normalize ctx l $ \l' -> 
      normalize ctx d $ \d' -> do
        registerType a
        n <- fresh
        LetArray n a l' d' <$> k (Var n a)

  S.ArrayGet i t -> 
    normalize ctx i $ \i' ->
      normalize ctx t $ \t' ->
        letBind (arrType t') (ArrayGet i' t') k

  S.ArraySet i t v -> 
    normalize ctx i $ \i' ->
      normalize ctx t $ \t' ->
        normalize ctx v $ \v' ->
          letBind (Array $ arrType t') (ArraySet i' t' v') k

  S.ArrayLen t -> 
    normalize ctx t $ \t' ->
      letBind Number (ArrayLen t') k

  S.Fold a t -> 
    normalize ctx t $ \t' -> do
      registerType a
      n <- fresh
      LetFold n a t' <$> k (Var n a)

  S.UnFold a t -> 
    normalize ctx t $ \t' -> 
      letBind a (UnFold t') k

  S.BoolLit b -> k (BoolLit b)

  S.If a v t u -> 
    normalize ctx v $ \v' -> do
      registerType a
      n <- fresh
      t' <- normalize ctx t (pure . Assign n)
      u' <- normalize ctx u (pure . Assign n)
      LetIf n a v' t' u' <$> k (Var n a)

  S.NumLit i -> k (NumLit i)

  S.ByteLit b -> k (ByteLit b)

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
  normalize ctx t $ \t' ->
    normalizeList ctx ts $ \ts' -> 
      k (t' : ts')

normalizePattern :: Context -> Pattern -> Val -> (Context -> ANF Term) -> ANF Term
normalizePattern ctx pat v k = do
  let ty = typeOf v
  case pat of
    PVar _ -> k (bindLocal v ctx)
    PTuple subs -> do
      registerType ty
      freshVals <- forM (prodType ty) $ \t -> pair <$> fresh <*> pure t
      LetMany freshVals v <$> bindSlots ctx (reverse $ zip subs freshVals)
  where
    bindSlots ctx' [] = k ctx'
    bindSlots ctx' ((subpat, fv) : rest) =
      normalizePattern ctx' subpat (uncurry Var fv) $ \ctx'' -> 
        bindSlots ctx'' rest

normalizeBranch :: Context -> Val -> (Val -> ANF Term) -> Int -> (Pattern, S.Term) -> ANF Term
normalizeBranch ctx v k i (pat, tm) = do
  let branchType = injType v i
  n <- fresh
  Let n branchType (Cast i v) <$>
    (normalizePattern ctx pat (Var n branchType) $ \ctx' -> 
      normalize ctx' tm k)


bindManyLocals :: [Name] -> [Type] -> Context -> Context
bindManyLocals ns ts ctx = foldr (bindLocal . uncurry Var) ctx (zip ns ts)

