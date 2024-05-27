module Options where

import Data.Char (toUpper)
import qualified Data.Map as M

invOption :: String -> String -> String
invOption opt s = "Invalid " ++ opt ++ " option value: " ++ s ++ "."

readMapGen :: Show a => [a] -> M.Map String [(a, [b])]
readMapGen xs = M.fromList $ uncurry (++) $ unzip
    [ let
        rpc = readsPrecFormat x
        opt = show x
      in
        ( (opt,        rpc)
        , ([head opt], rpc)
        )
    | x <- xs
    ]
  where readsPrecFormat x = [(x, [])]

data Options = Options
  { a, b, c :: Double
  , lang :: Language
  , form :: Format
  }

data Language = En | Ru
  deriving (Show, Eq)

instance Read Language
  where
    readsPrec _ s =
      case M.lookup s (readMapGen [Ru, En]) of
        Just x -> x
	Nothing -> error $ invOption "language" s

data Format = Std | Full
  deriving (Show, Eq)

instance Read Format
  where
    readsPrec _ s =
      case M.lookup s (readMapGen [Std, Full]) of
        Just x -> x
	Nothing -> error $ invOption "format" s

