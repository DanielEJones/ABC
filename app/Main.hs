module Main (main) where

import Control.Monad (forM, forM_)
import Control.Monad.Trans (liftIO)

import Text.Megaparsec (errorBundlePretty)
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
import Lowering.ANF hiding (Term, emptyContext, bindGlobal)

import Generation.CodeGen


fetchSource :: FilePath -> Query String
fetchSource path = cached dbSource path $ do
  content <- liftIO (readFile path)
  pure content

fetchParsed :: FilePath -> Query (Either Error [Sf.Decl])
fetchParsed path = cached dbParsed path $ do
  source <- fetchSource path
  let ast = doParse path source
  liftErr' ParseError ast

fetchSignature :: Ident -> Query (Either Error Sn.Sig)
fetchSignature ident@(Ident path name) = cachedFallible dbSigs ident $ do
  decls <- liftQuery (fetchParsed path)
  case findName name decls of
    Nothing -> undefined
    Just decl -> do 
      let sig = checkSig (emptyContext $ mkLoc path 0 0 ) decl
      liftErr TypeError sig

fetchBody :: Ident -> Query (Either Error Sf.Term)
fetchBody ident@(Ident path name) = cachedFallible dbDefs ident $ do
  decls <- liftQuery (fetchParsed path)
  case findName name decls of
    Nothing -> undefined
    Just decl -> pure (getTerm decl)

fetchContext :: FilePath -> Query (Either Error Sn.Context)
fetchContext path = cachedFallible dbCtx path $ do
  decls <- liftQuery (fetchParsed path)
  sigs <- forM decls (lowerQuery . fetchSignature . Ident path . nameOf)
  let emptyCtx = emptyContext (mkLoc path 0 0)
  pure . reduce sigs emptyCtx $ \s c -> case s of
    Right sig -> bindGlobal sig c
    _         -> c

fetchChecked :: Ident -> Query (Either Error Sn.Term)
fetchChecked ident@(Ident path _) = cachedFallible dbChecked ident $ do
  sig  <- liftQuery (fetchSignature ident)
  body <- liftQuery (fetchBody ident)    
  ctx  <- liftQuery (fetchContext path)
  let checked = checkDecl ctx body sig 
  liftErr TypeError checked

fetchANF :: Ident -> Query (Either Error Ir.Decl)
fetchANF ident@(Ident path _) = cachedFallible dbLowered ident $ do
  checked <- liftQuery (fetchChecked ident)
  signature <- liftQuery (fetchSignature ident)
  context <- liftQuery (fetchContext path)
  let lowered = lower context checked signature
  pure lowered

fetchCode :: Ident -> Query (Either Error String)
fetchCode ident = cachedFallible dbCodeGen ident $ do
  lowered <- liftQuery (fetchANF ident)
  let generated = emitDecl lowered
  pure generated

fetchProto :: Ident -> Query (Either Error String)
fetchProto ident = cachedFallible dbProtoGen ident $ do
  sig <- liftQuery (fetchSignature ident)
  let generated = emitSig sig
  pure generated

fetchCompiled :: FilePath -> Query (Either Error String)
fetchCompiled path = cachedFallible dbCompiled path $ do
  decls <- liftQuery (fetchParsed path)
  protos <- forM decls (liftQuery . fetchProto . Ident path . nameOf)
  code <- forM decls (liftQuery . fetchCode . Ident path . nameOf)
  pure $ unlines 
    [ "#include <stdio.h>\n"
    , unlines protos
    , unlines code
    , "int main() {"
    , "  int result = abc_main();"
    , "  printf(\"%d\\n\", result);"
    , "}"
    ]


main :: IO ()
main = do
  options <- getOptions
  putStrLn (show options)

  db <- emptyDatabase
  forM_ (optFiles options) $ \filePath -> do
    tree <- runQuery (fetchCompiled filePath) db
    case tree of
      Right a -> do putStrLn a; compileAndRun a
      Left e -> case e of
        ParseError pErr -> putStrLn (errorBundlePretty pErr)
        TypeError  tErr -> putStrLn (show tErr)


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

