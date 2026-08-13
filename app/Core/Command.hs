module Core.Command where

import System.Environment 


getOptions :: IO Options
getOptions = do
  args <- getArgs
  let opts = parseArgs args defaultOptions
  pure opts{ optFiles = reverse (optFiles opts) }

parseArgs :: [String] -> Options -> Options
parseArgs (file:rest) opts = parseArgs rest opts{ optFiles = file : optFiles opts }
parseArgs []          opts = opts

data Options = Options
  { optFiles :: [String]
  } deriving Show

defaultOptions :: Options
defaultOptions = Options []

