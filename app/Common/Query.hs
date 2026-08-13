{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common.Query where

import Control.Monad.Except
import Control.Monad.Reader

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map


type Store k v = IORef (Map k v)

data Database = Database
  { dbSource :: Store FilePath String
  }

emptyDatabase :: IO Database
emptyDatabase = Database
  <$> newIORef Map.empty


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

liftQuery :: Query a -> QueryFallible e a
liftQuery = QueryFallible . lift

