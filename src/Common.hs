module Common where

import Protolude
import qualified Data.Text as T
import Data.Char(digitToInt)

import qualified Data.Text.Read as TR
import Data.Foldable (maximumBy, Foldable)
import Data.Ord      (comparing)

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
