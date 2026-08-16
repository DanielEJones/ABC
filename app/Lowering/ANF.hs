module Lowering.ANF where

import Core.Common

import qualified Frontend.Syntax as S
import Frontend.Syntax hiding (Term(..), Context, emptyContext, bindLocal, bindGlobal)

import Control.Monad.State


--
-- Lowering Datatypes
--

data Decl
  = Fn Name [(Name, Type)] Type Term
  deriving Show

data Term 
  = Let Name Type Expr Term 
  | Ret Val
  deriving Show

data Expr
  = Arith Op Val Val
  | Call Name [Val]
  deriving Show

data Val
  = Var Name
  | NumLit Int
  deriving Show


-- 
-- Lowering Entry
--

lower :: S.Context -> S.Term -> Sig -> Decl
lower ctx tm (Sig n ps r) = do
  let anfCtx = Context [] (ctxGlob ctx)
  let fnCtx = foldl' (\c (p, _) -> bindLocal (Var p) c) anfCtx ps
  Fn n ps r (lowerTerm fnCtx tm)

lowerTerm :: Context -> S.Term -> Term
lowerTerm ctx tm = evalState (normalize ctx tm ret) 0


-- 
-- Lowering Terms
-- 

normalize :: Context -> S.Term -> (Val -> ANF Term) -> ANF Term
normalize ctx tm k = case tm of
  S.Let _ _ v u -> 
    normalize ctx v $ \v' ->
      normalize (bindLocal v' ctx) u k

  S.Call f ts -> 
    normalizeList ctx ts $ \ts' -> do
      n <- fresh
      Let n (getRetType f ctx) (Call f ts') <$> k (Var n)

  S.Var i -> k (getLocal i ctx)
  
  S.NumLit i -> k (NumLit i)

  S.Arith o t u -> 
    normalize ctx t $ \t' ->
      normalize ctx u $ \u' -> do
        n <- fresh
        Let n Number (Arith o t' u') <$> k (Var n)

normalizeList :: Context -> [S.Term] -> ([Val] -> ANF Term) -> ANF Term
normalizeList _ [] k = k []
normalizeList ctx (t:ts) k = 
  normalizeList ctx ts $ \ts' -> 
    normalize ctx t $ \t' ->
      k (t' : ts')

-- 
-- ANF Monad
--

type ANF a = State Int a

fresh :: ANF Name
fresh = do
  i <- state $ \s -> (s, s + 1)
  pure ("t" ++ show i)

ret :: Val -> ANF Term
ret = pure . Ret


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

