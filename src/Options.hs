module Options where

import Data.Char (toUpper)

data Options = Options
  { a, b, c :: Double
  , lang :: Language
  , form :: Format
  }

data Language = En | Ru
  deriving (Show, Eq)

instance Read Language
  where
    readsPrec _ str@(x:xs) =
      case map toUpper str of
        'R':xs -> [(Ru, [])]
        'E':xs -> [(En, [])]

data Format = Std | Full
  deriving (Show, Eq)

instance Read Format
  where
    readsPrec _ str@(x:xs) =
      case map toUpper str of
        'S':xs -> [(Std, [])]
        'F':xs -> [(Full, [])]
