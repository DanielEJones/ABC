module Core.Common where

import Text.Megaparsec (SourcePos (SourcePos), mkPos)


--
-- Common Data Types
--

type Name = String
type Ix = Int

data Op = Add | Sub | Mul | Div
  deriving Show

data Ident = Ident FilePath Name
  deriving (Show, Eq, Ord)


--
-- MegaParsec wrappers
--

type Loc = SourcePos

mkLoc :: String -> Int -> Int -> Loc
mkLoc name line col = SourcePos name (mkPos line) (mkPos col)


-- 
-- Other helpers
--

pair :: a -> b -> (a, b)
pair = (,)

reduce :: [a] -> b -> (a -> b -> b) -> b
reduce as b f = foldr f b as


class HasName a where
  nameOf :: a -> Name

findName :: HasName a => Name -> [a] -> Maybe a
findName _ [] = Nothing
findName n (x:xs)
  | nameOf x == n = Just x
  | otherwise = findName n xs

