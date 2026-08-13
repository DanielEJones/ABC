module Core.Common where

import Text.Megaparsec (SourcePos (SourcePos), mkPos)


--
-- Common Data Types
--

type Name = String
type Ix = Int

data Op = Add | Sub | Mul | Div
  deriving Show


--
-- MegaParsec wrappers
--

type Loc = SourcePos

mkLoc :: String -> Int -> Int -> Loc
mkLoc name line col = SourcePos name (mkPos line) (mkPos col)

