module Common where

import Protolude
import qualified Data.Text as T
import Data.Char(digitToInt)

import qualified Data.Text.Read as TR

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
