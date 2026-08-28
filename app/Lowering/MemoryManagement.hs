module Lowering.MemoryManagement where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Control.Monad (forM)

import Core.Common
import Frontend.Syntax (Type(..))
import Lowering.Core


type LiveSet = Map Name Type


insertMemoryOps :: Term -> ANF Term
insertMemoryOps tm = insertMemoryOps' Map.empty tm >>= (pure . snd)

insertMemoryOps' :: LiveSet -> Term -> ANF (LiveSet, Term)
insertMemoryOps' start tm = case tm of
  Let n t e k -> do
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live) 
      then pure (live, k')
      else do 
        (live', e', copies, drops) <- visitExpr live e
        pure (Map.delete n live', copies . Let n t e' . drops $ k')

  LetIf n t d u v k -> do
    -- `d` shouldn't ever be a heap type; it must type check to be a bool
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        (liveU, u') <- insertMemoryOps' live u
        (liveV, v') <- insertMemoryOps' live v
        let live' = Map.union liveU liveV
        let uDiff = Map.difference live' liveU
        let vDiff = Map.difference live' liveV
        let uWithDrop = Map.foldrWithKey dropVar u' uDiff
        let vWithDrop = Map.foldrWithKey dropVar v' vDiff
        pure (addToSet d . Map.delete n $ live', LetIf n t d uWithDrop vWithDrop k')

  LetMatch n t d bs k -> do
    -- `d` might be a heap type (sum holding ptr), so we need to consume it
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live) 
      then pure (live, k')
      else do
        (lives, bs') <- unzip <$> mapM (insertMemoryOps' live) bs
        let live' = foldr1 Map.union lives
        let bsWithDrop = flip map (zip lives bs') $ \(liveB, b') -> do
              let bDiff = Map.difference live' liveB
              Map.foldrWithKey dropVar b' bDiff

        (d', copy) <- case d of
          Var dn ty | Map.member dn live' && isHeapType ty -> do
            copyName <- fresh
            pure (Var copyName ty, Copy copyName d)
          other -> pure (other, id)

        pure (addToSet d . Map.delete n $ live', copy . LetMatch n t d' bsWithDrop $ k')

  LetArray n t l d k -> do
    -- `l` must always be an integer, never a heap type
    -- `d` must be copied `l` times if its a heap type, probably want to handle
    --   it specially inside the backend rather than inserting some weird copy loop
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        let drops = if (not $ containsVal live d) && isHeapVal d
              then Drop d
              else id
        pure (addToSet l . addToSet d . Map.delete n $ live, LetArray n t l d . drops $ k')

  LetArrayLit n t vs k -> do
    -- vs may need copying
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        (vs', needsCopy) <- fmap unzip . forM vs $ \val -> case val of
          Var vn ty | Map.member vn live && isHeapType ty -> do
            copyName <- fresh
            pure (Var copyName ty, Copy copyName val)
          other -> pure (other, id)

        let copies = flip (foldr ($)) needsCopy
        let live' = foldr addToSet live vs
        pure (Map.delete n live', copies . LetArrayLit n t vs' $ k')

  LetFold n t v k -> do
    -- Fold takes ownership of v, may need a copy
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        (v', copy) <- case v of 
          Var vn ty | Map.member vn live && isHeapType ty -> do
            copyName <- fresh
            pure (Var copyName ty, Copy copyName v)
          other -> pure (other, id)

        pure (addToSet v . Map.delete n $ live, copy . LetFold n t v' $ k')

  -- These both consume and terminate a block, Not sure they need anything
  Assign n v -> pure (addToSet v start, Assign n v)
  Ret v -> pure (addToSet v start, Ret v) 

  Copy{} -> error "Copy shouldn't be present in the pass that inserts copies!"
  Drop{} -> error "Drop shouldn't be present in the pass that inserts drops!"


--                                   live,    expr, copies,       drops
visitExpr :: LiveSet -> Expr -> ANF (LiveSet, Expr, Term -> Term, Term -> Term)
visitExpr live e = case e of
  -- These only apply to primitive ops which are guaranteed non-heap
  Arith _ l r -> pure (addToSet l . addToSet r $ live, e, id, id)
  Comp  _ l r -> pure (addToSet l . addToSet r $ live, e, id, id)
  Logic _ l r -> pure (addToSet l . addToSet r $ live, e, id, id)

  Call f args -> do
    -- Call consumes args, copy any that are used later. No drops.
    (args', needsCopy) <- fmap unzip . forM args $ \val -> case val of
      Var n ty | Map.member n live && isHeapType ty -> do
        copyName <- fresh
        pure (Var copyName ty, Just $ Copy copyName val)
      other -> pure (other, Nothing)
    let copies = flip (foldr $ \copy -> fromMaybe id copy) needsCopy
    let live' = foldr addToSet live args
    pure (live', Call f args', copies, id)

  Pair args -> do
    -- Pair consumes args, copy any that are used later. No drops.
    (args', needsCopy) <- fmap unzip . forM args $ \val -> case val of
      Var n ty | Map.member n live && isHeapType ty -> do
        copyName <- fresh
        pure (Var copyName ty, Copy copyName val)
      other -> pure (other, id)
    let copies = flip (foldr ($)) needsCopy
    let live' = foldr addToSet live args
    pure (live', Pair args', copies, id)

  Proj i v -> do
    -- Proj borrows the pair, internal copies only, maybe drop the pair.
    let drops = if (not $ containsVal live v) && isHeapVal v
          then Drop v -- unseen thus far, ie last use: drop
          else id     -- has a later use: keep
    pure (addToSet v live, Proj i v, id, drops)

  Inj i v -> do
    -- Inj consumes v, copy if used later. No drops.
    (arg, copy) <- case v of
      Var n ty | Map.member n live && isHeapType ty -> do
        copyName <- fresh
        pure (Var copyName ty, Copy copyName v)
      other -> pure (other, id)
    pure (addToSet v live, Inj i arg, copy, id)

  ArrayGet i v -> do
    -- ArrayGet borrows v, internal copies only, maybe drops v
    let drops = if (not $ containsVal live v) && isHeapVal v
          then Drop v
          else id
    pure (addToSet i . addToSet v $ live, ArrayGet i v, id, drops)

  ArraySet i v a -> do
    -- ArraySet consumes both the array and the value
    (v', copyV) <- case v of
      Var n ty | Map.member n live && isHeapType ty -> do
        copyName <- fresh
        pure (Var copyName ty, Copy copyName v)
      other -> pure (other, id)

    (a', copyA) <- case a of
      Var n ty | Map.member n live && isHeapType ty -> do
        copyName <- fresh
        pure (Var copyName ty, Copy copyName a)
      other -> pure (other, id)
    
    pure (addToSet i . addToSet v . addToSet a $ live, ArraySet i v' a', copyV . copyA, id)

  ArrayLen v -> do
    -- ArrayLen borrows v, no copies, maybe drops v
    let drops = if (not $ containsVal live v) && isHeapVal v
          then Drop v
          else id
    pure (addToSet v live, ArrayLen v, id, drops)

  UnFold v -> do
    -- Dereferencing the pointer consumes it, maybe a copy, no (external) drops.
    (v', copy) <- case v of
      Var n ty | Map.member n live && isHeapType ty -> do
        copyName <- fresh
        pure (Var copyName ty, Copy copyName v)
      other -> pure (other, id)
    pure (addToSet v live, UnFold v', copy, id) 

  Cast i v -> do
    -- Cast is a compiler-inserted op that has no effect on memory management.
    -- Anything it could do is handled by LetCase.
    pure (live, Cast i v, id, id)


addToSet :: Val -> LiveSet -> LiveSet
addToSet (Var n t) live = Map.insert n t live 
addToSet _         live = live


containsVal :: LiveSet -> Val -> Bool
containsVal live (Var n _) = Map.member n live
containsVal _    _         = False

isHeapVal :: Val -> Bool
isHeapVal (Var _ t) = isHeapType t
isHeapVal _         = False


dropVar :: Name -> Type -> Term -> Term
dropVar name ty
  | isHeapType ty = Drop (Var name ty)
  | otherwise = id

isHeapType :: Type -> Bool
isHeapType Fix{}     = True
isHeapType Array{}   = True
isHeapType (Prod ts) = any isHeapType ts
isHeapType (Sum ts)  = any isHeapType ts
isHeapType _         = False
