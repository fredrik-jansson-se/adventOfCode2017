module Common where

import Protolude
import qualified Data.Text as T
import Data.Char(digitToInt)

import qualified Data.Text.Read as TR
import Data.Foldable (maximumBy, Foldable)
import Data.Ord      (comparing)

import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number

toInts :: Text -> [Int]
toInts t = map digitToInt s
  where
    s = T.unpack t

parseInt :: Int -> Text -> Int
parseInt def txt = let
  r = TR.signed TR.decimal
  in
    case r txt of
      Left _ -> def
      Right (i, _) -> i

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

csv :: Text -> [Int]
csv txt =  case P.parse line "csv" txt of
  Left _ -> []
  Right v -> v
  where
    num :: Parsec Text () Int
    num = do
      v <- int
      skipMany $ P.char ','
      spaces
      return v
      
    line :: Parsec Text () [Int]
    line = many1 num

