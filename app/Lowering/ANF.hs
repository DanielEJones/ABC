module Lowering.ANF where

import Core.Common

import qualified Frontend.Syntax as S
import Frontend.Syntax hiding (Term(..), Context, emptyContext)

import Control.Monad.State


--
-- Lowering Datatypes
--

data Term 
  = Let Name Type Expr Term 
  | Ret Val
  deriving Show

data Expr
  = Arith Op Val Val
  deriving Show

data Val
  = Var Name
  | NumLit Int
  deriving Show


-- 
-- Lowering Entry
--

lower :: S.Term -> Term
lower tm = evalState (normalize emptyContext tm ret) 0


-- 
-- Lowering Terms
-- 

normalize :: Context -> S.Term -> (Val -> ANF Term) -> ANF Term
normalize ctx tm k = case tm of
  S.Let _ _ v u -> 
    normalize ctx v $ \v' ->
      normalize (bind v' ctx) u k

  S.Var i -> k (getBound i ctx)
  
  S.NumLit i -> k (NumLit i)

  S.Arith o t u -> 
    normalize ctx t $ \t' ->
      normalize ctx u $ \u' -> do
        n <- fresh
        Let n Number (Arith o t' u') <$> k (Var n)


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
  { ctxBound :: [Val]
  }

emptyContext :: Context
emptyContext = Context []

bind :: Val -> Context -> Context
bind v ctx = ctx{ ctxBound = v : ctxBound ctx }

getBound :: Ix -> Context -> Val
getBound ix ctx = ctxBound ctx !! ix

