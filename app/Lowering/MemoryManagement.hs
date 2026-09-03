module Lowering.MemoryManagement where


import Data.Map (Map)
import qualified Data.Map as Map

import Core.Common
import Frontend.Syntax (Type(..))
import Lowering.Core


type LiveSet = Map Name Type


insertMemoryOps :: [Val] -> Term -> ANF Term
insertMemoryOps params tm = do 
  (liveSet, term) <- insertMemoryOps' Map.empty tm
  let termWithDroppedParams = reduce params term $ \param -> 
        if Map.notMember (nameOf param) liveSet && isHeapVal param
            then Drop param
            else id
  pure (termWithDroppedParams)

insertMemoryOps' :: LiveSet -> Term -> ANF (LiveSet, Term)
insertMemoryOps' start tm = case tm of
  Let n t e k -> do
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live) 
      then pure (live, k')
      else do 
        (live', e', copies, drops) <- visitExpr live e
        pure (Map.delete n live', copies . Let n t e' . drops $ k')

  LetMany ns v k -> do
    -- ns may need dropping if 
    (live, k') <- insertMemoryOps' start k
    let names = map fst ns
    if all (flip Map.notMember live) names
      then pure (live, k')
      else do
        let withDrops = reduce (map (uncurry Var) ns) k' $ \val ->
              if Map.notMember (nameOf val) live && isHeapVal val
                  then Drop val
                  else id

        (live', v', copies) <- consumeVal live v
        pure (foldr Map.delete live' names, copies . LetMany ns v' $ withDrops)

  LetIf n t d u v k -> do
    -- `d` shouldn't ever be a heap type; it must type check to be a bool
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        let liveBranches = Map.delete n live
        (liveU, u') <- insertMemoryOps' liveBranches u
        (liveV, v') <- insertMemoryOps' liveBranches v
        let live' = Map.union liveU liveV
        let uDiff = Map.difference live' liveU
        let vDiff = Map.difference live' liveV
        let uWithDrop = Map.foldrWithKey dropVar u' uDiff
        let vWithDrop = Map.foldrWithKey dropVar v' vDiff
        pure (addToSet d live', LetIf n t d uWithDrop vWithDrop k')

  LetMatch n t d bs k -> do
    -- `d` might be a heap type (sum holding ptr), so we need to consume it
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live) 
      then pure (live, k')
      else do
        let liveBranches = Map.delete n live
        (lives, bs') <- unzip <$> mapM (insertMemoryOps' liveBranches) bs
        let live' = foldr1 Map.union lives
        let bsWithDrop = flip map (zip lives bs') $ \(liveB, b') -> do
              let bDiff = Map.difference live' liveB
              Map.foldrWithKey dropVar b' bDiff

        (live'', d', copy) <- consumeVal live' d
        pure (live'', copy . LetMatch n t d' bsWithDrop $ k')

  LetArray n t l d k -> do
    -- `l` must always be an integer, never a heap type
    -- `d` must be copied `l` times if its a heap type, probably want to handle
    --   it specially inside the backend rather than inserting some weird copy loop
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        (live', d', drops) <- borrowVal live d
        pure (addToSet l . Map.delete n $ live', LetArray n t l d' . drops $ k')

  LetArrayLit n t vs k -> do
    -- vs may need copying
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        (live', vs', copies) <- consumeList live vs
        pure (Map.delete n live', copies . LetArrayLit n t vs' $ k')

  LetFold n t v k -> do
    -- Fold takes ownership of v, may need a copy
    (live, k') <- insertMemoryOps' start k
    if not (Map.member n live)
      then pure (live, k')
      else do 
        (live', v', copies) <- consumeVal live v
        pure (Map.delete n live', copies . LetFold n t v' $ k')

  -- These both consume and terminate a block, Not sure they need anything, but
  -- they are operations that consume so we'll call consume anyway.
  Assign n v -> do 
    (live', v', copies) <- consumeVal start v
    pure (live', copies $ Assign n v')

  Ret v -> do 
    (live', v', copies) <- consumeVal start v
    pure (live', copies $ Ret v') 

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
    (live', args', copies) <- consumeList live args
    pure (live', Call f args', copies, id)

  Pair args -> do
    -- Pair consumes args, copy any that are used later. No drops.
    (live', args', copies) <- consumeList live args
    pure (live', Pair args', copies, id)

  Proj i v -> do
    -- Proj borrows the pair, internal copies only, maybe drop the pair.
    (live', v', drops) <- borrowVal live v
    pure (live', Proj i v', id, drops)

  Inj i v -> do
    -- Inj consumes v, copy if used later. No drops.
    (live', v', copies) <- consumeVal live v
    pure (live', Inj i v', copies, id)

  ArrayGet i v -> do
    -- ArrayGet borrows v, internal copies only, maybe drops v
    (live', v', drops) <- borrowVal live v
    pure (addToSet i live', ArrayGet i v', id, drops)

  ArraySet i v a -> do
    -- ArraySet consumes both the array and the value
    (liveAfterV, v', copyV) <- consumeVal live v
    (liveAfterA, a', copyA) <- consumeVal liveAfterV a
    pure (addToSet i liveAfterA, ArraySet i v' a', copyV . copyA, id)

  ArrayLen v -> do
    -- ArrayLen borrows v, no copies, maybe drops v
    (live', v', drops) <- borrowVal live v
    pure (live', ArrayLen v', id, drops)

  UnFold v -> do
    -- Dereferencing the pointer consumes it, maybe a copy, no (external) drops.
    (live', v', copies) <- consumeVal live v
    pure (live', UnFold v', copies, id) 

  Cast i v -> do
    -- Cast is a compiler-inserted op that has no effect on memory management.
    -- Anything it could do is handled by LetCase.
    pure (live, Cast i v, id, id)


consumeVal :: LiveSet -> Val -> ANF (LiveSet, Val, Term -> Term)
consumeVal live val = case val of
  Var n ty | Map.member n live && isHeapType ty -> do
    copyName <- fresh
    pure (live, Var copyName ty, Copy copyName val)
  other -> do
    pure (addToSet val live, other, id)

borrowVal :: LiveSet -> Val -> ANF (LiveSet, Val, Term -> Term)
borrowVal live val = case val of
  Var n ty | Map.notMember n live && isHeapType ty -> do
    pure (addToSet val live, val, Drop val)
  other -> do
    pure (addToSet val live, other, id)

consumeList :: LiveSet -> [Val] -> ANF (LiveSet, [Val], Term -> Term)
consumeList live [] = pure (live, [], id)
consumeList live (val:rest) = do
  (liveRest, restVals, restCopies) <- consumeList live rest
  (live', val', copy) <- consumeVal liveRest val
  pure (live', val' : restVals, copy . restCopies)

dropVar :: Name -> Type -> Term -> Term
dropVar name ty
  | isHeapType ty = Drop (Var name ty)
  | otherwise = id


addToSet :: Val -> LiveSet -> LiveSet
addToSet (Var n t) live = Map.insert n t live 
addToSet _         live = live

containsVal :: LiveSet -> Val -> Bool
containsVal live (Var n _) = Map.member n live
containsVal _    _         = False

isHeapVal :: Val -> Bool
isHeapVal (Var _ t) = isHeapType t
isHeapVal _         = False

isHeapType :: Type -> Bool
isHeapType Fix{}     = True
isHeapType Array{}   = True
isHeapType (Prod ts) = any isHeapType ts
isHeapType (Sum ts)  = any isHeapType ts
isHeapType _         = False

