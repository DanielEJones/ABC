module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Text.Megaparsec (errorBundlePretty)

import Core.Command
import Core.Query

import Frontend.Surface


fetchSource :: FilePath -> Query String
fetchSource path = cached dbSource path $ do
  content <- liftIO (readFile path)
  pure content

fetchParsed :: FilePath -> Query (Either Error Term)
fetchParsed path = cached dbParsed path $ do
  source <- fetchSource path
  let ast = doParse path source
  pure ast


main :: IO ()
main = do
  options <- getOptions
  putStrLn (show options)

  db <- emptyDatabase
  forM_ (optFiles options) $ \filePath -> do
    tree <- runQuery (fetchParsed filePath) db
    case tree of
      Left err -> putStrLn (errorBundlePretty err)
      Right tm -> putStrLn (show tm)

