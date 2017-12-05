{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Protolude

import qualified Data.Text as T
import qualified Data.Set as Set

isValid :: [Text] -> Bool
isValid txt = length txt == Set.size set
  where
    set = Set.fromList txt

isValid2 :: [Text] -> Bool
isValid2 txt = length sorted == Set.size ssorted
  where
    tsort :: Text -> Text
    tsort = T.pack . sort . T.unpack
    sorted = map tsort txt
    ssorted = Set.fromList sorted


solve1 :: Text -> Int
solve1 txt = let
  lin = T.lines txt
  wrds = map T.words lin
  valids = filter identity $ map isValid wrds
  in
    length valids

solve2 :: Text -> Int
solve2 txt = let
  lin = T.lines txt
  wrds = map T.words lin
  valids = filter identity $ map isValid2 wrds
  in
    length valids
  
