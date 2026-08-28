module Lowering.Core where

import Control.Monad.State

import Core.Common
import Frontend.Syntax (Type(..), Sig(..), unfoldType)


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
  | LetArray Name Type Val Val Term
  | LetArrayLit Name Type [Val] Term
  | LetFold Name Type Val Term
  | Assign Name Val
  | Copy Name Val Term
  | Drop Val Term
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
  | ArrayGet Val Val
  | ArraySet Val Val Val
  | ArrayLen Val
  | UnFold Val
  | Cast Int Val
  deriving Show

data Val
  = Var Name Type
  | NumLit Int
  | BoolLit Bool
  deriving Show


-- 
-- ANF Monad
--

type ANF a = State LoweringState a

data LoweringState = LoweringState
  { loweredCount :: Int
  , loweredTypes :: [Type]
  , loweredFixes :: [Type]
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

addType :: Type -> ANF ()
addType t = modify $ \s -> s{ loweredTypes = loweredTypes s ++ [t] }

addSeen :: Type -> ANF ()
addSeen t = modify $ \s -> s{ loweredFixes = t : loweredFixes s }

hasSeen :: Type -> ANF Bool
hasSeen t = do s <- get; pure (t `elem` loweredFixes s)

registerType :: Type -> ANF ()
registerType t@(Prod as) = do mapM_ registerType as; addType t
registerType t@(Sum as)  = do mapM_ registerType as; addType t
registerType t@(Array a) = do registerType a; addType t
registerType t@Fix{}    = do 
  seen <- hasSeen t
  if (not seen)
    then do 
      addSeen t 
      addType t
      registerType (unfoldType t) 
    else pure ()
registerType TypeVar{} = error "Should never try to find type of unbound var"
registerType _ = pure ()

getTypesOf :: [Type] -> Type -> [Type]
getTypesOf seen t@(Prod as) = concatMap (getTypesOf seen) as ++ [t]
getTypesOf seen t@(Sum as)  = concatMap (getTypesOf seen) as ++ [t]
getTypesOf seen t@(Array a) = getTypesOf seen a ++ [t]
getTypesOf seen t@Fix{}
  | t `elem` seen = []
  | otherwise = t : getTypesOf (t : seen) (unfoldType t) 
getTypesOf _    TypeVar{}   = error "Should never find type of unbound var"
getTypesOf _    _           = []

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

instance HasName Val where
  nameOf (Var n _) = n
  nameOf _         = error "Sucks to suck"

projType :: Val -> Int -> Type
projType (Var _ (Prod ts)) i = ts !! i
projType _                 _ = error "Terms should be well typed (project)"
  
injType :: Val -> Int -> Type
injType (Var _ (Sum ts)) i = ts !! i
injType _                _ = error "Terms should be well typed (inject)"

arrType :: Val -> Type
arrType (Var _ (Array t)) = t
arrType other             = error ("Terms should be well typed (array): " ++ show other)

