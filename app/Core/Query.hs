{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Query where

import Control.Monad.Except
import Control.Monad.Reader

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Frontend.Surface as Sf
import qualified Frontend.Syntax as Sn
import qualified Lowering.ANF as Ir


type Store k v = IORef (Map k v)

data Database = Database
  { dbSource    :: Store FilePath String
  , dbParsed    :: Store FilePath (Either Error Sf.Term)
  , dbChecked   :: Store FilePath (Either Error Sn.Term)
  , dbLowered   :: Store FilePath (Either Error Ir.Term)
  , dbGenerated :: Store FilePath (Either Error String)
  }

emptyDatabase :: IO Database
emptyDatabase = Database
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty


-- 
-- Pure Queries
--

newtype Query a = Query{ unQuery :: ReaderT Database IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Database)

runQuery :: Query a -> Database -> IO a
runQuery = runReaderT . unQuery

cached :: Ord k => (Database -> Store k v) -> k -> Query v -> Query v
cached store key action = do
  ref   <- asks store
  cache <- liftIO (readIORef ref)
  case Map.lookup key cache of
    Just v -> pure v
    Nothing -> do
      v <- action
      liftIO (modifyIORef' ref $ Map.insert key v)
      pure v


--
-- Fallible Queries
--

newtype QueryFallible e a = QueryFallible { unFallible :: ExceptT e Query a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Database, MonadError e)

runQueryFallible :: QueryFallible e a -> Database -> IO (Either e a)
runQueryFallible = runQuery . runExceptT . unFallible

cachedFallible :: Ord k => (Database -> Store k (Either e v)) -> k -> QueryFallible e v -> Query (Either e v)
cachedFallible store key = cached store key . runExceptT . unFallible

lowerQuery :: Query a -> QueryFallible e a
lowerQuery = QueryFallible . lift

liftQuery :: Query (Either e a) -> QueryFallible e a
liftQuery q = do
  res <- lowerQuery q
  case res of
    Left e  -> throwError e
    Right a -> pure a

--
-- Error
--

data Error
  = ParseError Sf.Error
  | TypeError  Sn.Error
  deriving Show

liftErr' :: (e -> Error) -> Either e a -> Query (Either Error a)
liftErr' lifter e = pure $ case e of
  Left err -> Left (lifter err)
  Right a  -> Right a

liftErr :: (e -> Error) -> Either e a -> QueryFallible Error a
liftErr lifter e = case e of
  Left err -> throwError (lifter err)
  Right a  -> pure a

