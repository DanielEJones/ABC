module Frontend.Surface where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Core.Common


--
-- Surface Datatypes
--

data Term
  = TmLoc Loc Term

  | Let Name Type Term Term
  | Var Name

  | NumLit Int
  | Arith Op Term Term
  deriving Show

data Type
  = TyLoc Loc Type
  | Number
  deriving Show
  
class HasLocation a where
  locate :: Loc -> a -> a

instance HasLocation Term where
  locate = TmLoc

instance HasLocation Type where
  locate = TyLoc


--
-- Term Parser
--

parseLet :: Parser Term
parseLet = parens . into' "let" $ 
  Let <$> parseAtom
      <*> parseType
      <*> parseTerm
      <*> parseTerm
  
parseVar :: Parser Term
parseVar = Var <$> parseAtom

parseNumLit :: Parser Term
parseNumLit = NumLit <$> parseRawNumber

parseArith :: Parser Term
parseArith = parens $
  Arith <$> parseOp
        <*> parseTerm
        <*> parseTerm

parseTerm :: Parser Term
parseTerm = located (try parseLet <|> parseArith <|> parseNumLit <|> parseVar)


--
-- Type Parser
--

parseNumber :: Parser Type
parseNumber = "int" `into` Number

parseType :: Parser Type
parseType = parseNumber


--
-- Entry Point
--

doParse :: FilePath -> String -> ParseResult Term
doParse = parse (sc *> parseTerm <* sc <* eof) 


--
-- MegaParsec Boilerplate
--

type Parser = Parsec Error' String
type Error  = ParseErrorBundle String Error'
type ParseResult = Either Error

-- Allows us to report custom errors in a megaparsec error bundle
data Error' = Error'
  deriving (Show, Eq, Ord)

instance ShowErrorComponent Error' where
  showErrorComponent = show

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")") 

braces :: Parser a -> Parser a
braces = between (symbol "[") (symbol "]")

located :: HasLocation a => Parser a -> Parser a
located = (<*>) (locate <$> lexeme getSourcePos)

into :: String -> a -> Parser a
into s x = symbol s *> pure x

into' :: String -> Parser a -> Parser a
into' s p = symbol s *> p

parseAtom :: Parser String
parseAtom = lexeme (some $ alphaNumChar <|> oneOf "_!@#$%&*-+=:/?<>")

parseRawNumber :: Parser Int
parseRawNumber = lexeme $ do
  chars <- some numberChar
  pure (read chars)

parseOp :: Parser Op
parseOp = 
      "+" `into` Add 
  <|> "-" `into` Sub 
  <|> "*" `into` Mul 
  <|> "/" `into` Div

