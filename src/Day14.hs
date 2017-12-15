{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day14 where

import Protolude

import qualified Day10 as D10
import qualified Data.Text as T
import TextShow (showt)

append :: Text -> Int -> Text
append txt idx = T.concat [txt, "-", showt idx]

inputs :: Text -> [Text]
inputs key = map (append key) [0..127]

bits :: Char -> Int
bits '0' = 0
bits '1' = 1
bits '2' = 1
bits '3' = 2
bits '4' = 1
bits '5' = 2
bits '6' = 2
bits '7' = 3
bits '8' = 1
bits '9' = 2
bits 'a' = 2
bits 'b' = 3
bits 'c' = 2
bits 'd' = 3
bits 'e' = 3
bits 'f' = 4

cntBits :: Text -> Int
cntBits txt = cb 0 $ T.unpack txt
  where
    cb acc [] = acc
    cb acc (x:xs) = cb (acc + bits x) xs

solve1 :: Text -> Int
solve1 key = let
  inp = inputs key
  hashs = map (D10.hash 256) inp
  cnts = map cntBits hashs
  in 
    sum cnts
