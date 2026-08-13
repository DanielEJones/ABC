module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)

import Common.Command
import Common.Query


fetchSource :: FilePath -> Query String
fetchSource path = cached dbSource path $ do
  content <- liftIO (readFile path)
  pure content


main :: IO ()
main = do
  options <- getOptions
  db <- emptyDatabase

  forM_ (optFiles options) $ \filePath -> do
    content <- runQuery (fetchSource filePath) db
    putStrLn content

