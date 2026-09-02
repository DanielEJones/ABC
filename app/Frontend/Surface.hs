module Frontend.Surface where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Core.Common


--
-- Surface Datatypes
--

data Decl
  = DLoc Loc Decl
  | FnDef Name [(Name, Type)] Type Term
  deriving Show

data Term
  = TmLoc Loc Term

  | Let Name Type Term Term
  | Call Name [Term]
  | Var Name

  | Pair [Term]
  | Proj Int Term

  | Inj Int Term
  | Match Term [(Name, Term)]

  | ArrayLit [Term]
  | ArrayNew Term Term
  | ArrayGet Term Term
  | ArraySet Term Term Term
  | ArrayLen Term

  | Fold Term
  | UnFold Term

  | BoolLit Bool
  | If Term Term Term

  | NumLit Int
  | CharLit Char
  | StringLit String

  | Arith AOp Term Term
  | Comp COp Term Term
  | Logic LOp Term Term
  deriving Show

data Type
  = TyLoc Loc Type
  | Number
  | Boolean
  | Byte
  | Prod [Type]
  | Sum [Type]
  | Array Type
  | Fix Name Type
  | TypeVar Name
  deriving Show
  

class HasLocation a where
  locate :: Loc -> a -> a

instance HasLocation Decl where
  locate = DLoc

instance HasLocation Term where
  locate = TmLoc

instance HasLocation Type where
  locate = TyLoc

instance HasName Decl where
  nameOf (DLoc _ d)      = nameOf d
  nameOf (FnDef n _ _ _) = n


--
-- Term Parser
--

parseLet :: Parser Term
parseLet = parens . into' "let" $ 
  Let <$> parseAtom
      <*> parseType
      <*> parseTerm
      <*> parseTerm

parseCall :: Parser Term
parseCall = parens $ 
  Call <$> parseAtom
       <*> many parseTerm
  
parseVar :: Parser Term
parseVar = Var <$> parseAtom

parsePair :: Parser Term
parsePair = parens . into' "pair" $
  Pair <$> many parseTerm

parseProj :: Parser Term
parseProj = parens . into' "project" $
  Proj <$> parseRawNumber
       <*> parseTerm

parseInj :: Parser Term
parseInj = parens . into' "inject" $
  Inj <$> parseRawNumber
      <*> parseTerm

parseMatch :: Parser Term
parseMatch = parens . into' "match" $
  Match <$> parseTerm
        <*> many (parens $ pair <$> braces parseAtom <*> parseTerm)

parseArrayLit :: Parser Term
parseArrayLit = braces (ArrayLit <$> many parseTerm)

parseArrayNew :: Parser Term
parseArrayNew = parens . into' "array-new" $
  ArrayNew <$> parseTerm
           <*> parseTerm

parseArrayGet :: Parser Term
parseArrayGet = parens . into' "array-get" $
  ArrayGet <$> parseTerm
           <*> parseTerm

parseArraySet :: Parser Term
parseArraySet = parens . into' "array-set" $
  ArraySet <$> parseTerm
           <*> parseTerm
           <*> parseTerm

parseArrayLen :: Parser Term
parseArrayLen = parens . into' "array-len" $
  ArrayLen <$> parseTerm

parseFold :: Parser Term
parseFold = parens . into' "fold" $
  Fold <$> parseTerm

parseUnFold :: Parser Term
parseUnFold = parens . into' "unfold" $
  UnFold <$> parseTerm

parseBoolLit :: Parser Term
parseBoolLit = 
      "true"  `into` BoolLit True
  <|> "false" `into` BoolLit False

parseIf :: Parser Term
parseIf = parens . into' "if" $
  If <$> parseTerm
     <*> parseTerm
     <*> parseTerm

parseNumLit :: Parser Term
parseNumLit = NumLit <$> parseRawNumber

parseCharLit :: Parser Term
parseCharLit = lexeme $ do
  _ <- char '\''
  ch <- L.charLiteral
  _ <- char '\''
  pure (CharLit ch)

parseStringLit :: Parser Term
parseStringLit = lexeme $ do
  body <- char '"' *> manyTill L.charLiteral (char '"')
  pure (StringLit body)

parseArith :: Parser Term
parseArith = parens $
  Arith <$> parseAOp
        <*> parseTerm
        <*> parseTerm

parseComp :: Parser Term
parseComp = parens $
  Comp <$> parseCOp
       <*> parseTerm
       <*> parseTerm

parseLogic :: Parser Term
parseLogic = parens $
  Logic <$> parseLOp
        <*> parseTerm
        <*> parseTerm

parseTerm :: Parser Term
parseTerm = located $ 
      try parseLet 
  <|> try parsePair
  <|> try parseProj
  <|> try parseInj
  <|> try parseMatch
  <|> parseArrayLit
  <|> try parseArrayNew
  <|> try parseArrayGet
  <|> try parseArraySet
  <|> try parseArrayLen
  <|> try parseFold
  <|> try parseUnFold
  <|> try parseIf
  <|> try parseComp
  <|> try parseArith 
  <|> try parseLogic 
  <|> parseCall 
  <|> parseBoolLit
  <|> parseNumLit 
  <|> parseCharLit
  <|> parseStringLit
  <|> parseVar


--
-- Type Parser
--

parseNumber :: Parser Type
parseNumber = "int" `into` Number

parseByte :: Parser Type
parseByte = "byte" `into` Byte

parseBoolean :: Parser Type
parseBoolean = "bool" `into` Boolean

parseProd :: Parser Type
parseProd = parens . into' "&" $ 
  Prod <$> many parseType

parseSum :: Parser Type
parseSum = parens . into' "|" $
  Sum <$> many parseType

parseArray :: Parser Type
parseArray = parens . into' "array" $
  Array <$> parseType

parseFix :: Parser Type
parseFix = parens . into' "fix" $
  Fix <$> parseAtom
      <*> parseType

parseTypeVar :: Parser Type
parseTypeVar = TypeVar <$> parseAtom

parseType :: Parser Type
parseType = located $ 
      parseNumber 
  <|> parseByte
  <|> parseBoolean
  <|> try parseProd 
  <|> try parseSum 
  <|> try parseArray
  <|> try parseFix
  <|> try parseTypeVar


-- 
-- Declaration Parser
--

parseParam :: Parser (Name, Type)
parseParam = parens (pair <$> parseAtom <*> parseType)

parseParams :: Parser [(Name, Type)]
parseParams = braces (many parseParam)

parseFnDef :: Parser Decl
parseFnDef = parens . into' "define" $
  FnDef <$> parseAtom
        <*> parseParams
        <*> parseType
        <*> parseTerm

parseDecl :: Parser Decl
parseDecl = located parseFnDef


--
-- Entry Point
--

doParse :: FilePath -> String -> ParseResult [Decl]
doParse = parse (sc *> many parseDecl <* sc <* eof) 


--
-- MegaParsec Boilerplate
--

type Parser = Parsec Error' String
type Error  = ParseErrorBundle String Error'
type ParseResult = Either Error

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

parseAOp :: Parser AOp
parseAOp = 
      "+" `into` Add 
  <|> "-" `into` Sub 
  <|> "*" `into` Mul 
  <|> "/" `into` Div

parseCOp :: Parser COp
parseCOp = 
      "="  `into` Eq
  <|> "/=" `into` NEq
  <|> "<=" `into` LtE
  <|> "<"  `into` Lt
  <|> ">=" `into` GtE
  <|> ">"  `into` Gt

parseLOp :: Parser LOp
parseLOp = 
      "and" `into` And
  <|> "or"  `into` Or


-- 
-- Helpers
--

getTerm :: Decl -> Term
getTerm (DLoc _ d)      = getTerm d
getTerm (FnDef _ _ _ t) = t

