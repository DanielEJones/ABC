module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)

import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit

import Core.Command
import Core.Common
import Core.Query

import qualified Frontend.Surface as Sf
import Frontend.Surface hiding (Term, Type, Error)

import qualified Frontend.Syntax as Sn
import Frontend.Syntax hiding (Term, Type, Error)

import qualified Lowering.ANF as Ir
import Lowering.ANF hiding (Term, emptyContext)

import Generation.CodeGen


fetchSource :: FilePath -> Query String
fetchSource path = cached dbSource path $ do
  content <- liftIO (readFile path)
  pure content

fetchParsed :: FilePath -> Query (Either Error Sf.Term)
fetchParsed path = cached dbParsed path $ do
  source <- fetchSource path
  let ast = doParse path source
  liftErr' ParseError ast

fetchChecked :: FilePath -> Query (Either Error Sn.Term)
fetchChecked path = cachedFallible dbChecked path $ do
  parsed <- liftQuery (fetchParsed path)
  let checked = check (emptyContext $ mkLoc path 0 0) parsed Sn.Number
  liftErr TypeError checked

fetchANF :: FilePath -> Query (Either Error Ir.Term)
fetchANF path = cachedFallible dbLowered path $ do
  checked <- liftQuery (fetchChecked path)
  let lowered = lower checked
  pure lowered

fetchCode :: FilePath -> Query (Either Error String)
fetchCode path = cachedFallible dbGenerated path $ do
  lowered <- liftQuery (fetchANF path)
  let generated = emit lowered
  pure generated


main :: IO ()
main = do
  options <- getOptions
  putStrLn (show options)

  db <- emptyDatabase
  forM_ (optFiles options) $ \filePath -> do
    tree <- runQuery (fetchCode filePath) db
    case tree of
      Left e  -> putStrLn (show e)
      Right a -> compileAndRun a


compileAndRun :: String -> IO ()
compileAndRun code = withSystemTempDirectory "abc-compiler-run" $ \dir -> do
  let cFile = dir ++ "/program.c"; exe = dir ++ "/program"

  writeFile cFile code
  (compEx, _, compErr) <- readProcessWithExitCode "cc" [cFile, "-o", exe] ""
  
  case compEx of
    ExitFailure{} -> putStrLn compErr
    ExitSuccess -> do
      (runEx, runOut, runErr) <- readProcessWithExitCode exe [] ""
      case runEx of
        ExitFailure{} -> putStrLn runErr
        ExitSuccess   -> putStr runOut

