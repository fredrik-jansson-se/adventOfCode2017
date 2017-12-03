module Common where

import Protolude
import qualified Data.Text as T
import Data.Char(digitToInt)

toInts :: Text -> [Int]
toInts t = map digitToInt s
  where
    s = T.unpack t

