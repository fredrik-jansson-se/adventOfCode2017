{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day12 where

import Protolude
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number
import qualified Data.Map.Strict as M
import qualified Data.IntSet as S

-- 2 <-> 0, 3, 4
line :: Parsec Text () (Int, [Int])
line = do
  k <- int
  spaces
  string "<->"
  spaces
  vals <- many1 num
  newline
  return (k, vals)
  where
    num :: Parsec Text () Int
    num = do
      v <- int
      skipMany $ P.char ','
      skipMany $ P.char ' '
      return v

parse :: Text -> [(Int, [Int])]
parse txt = 
  case P.parse (many1 line) "day12" txt of
    Left err -> traceShow err []
    Right xs -> xs

type NBR = M.Map Int [Int]

addNBR :: Int -> Int -> NBR -> NBR
addNBR a b m = let
  f key (new_value:_) old_value = new_value:old_value
  (_, m') = M.insertLookupWithKey f a [b] m
  (_, m'') = M.insertLookupWithKey f b [a] m'
  in m''

addNBRS :: Int -> [Int] -> NBR -> NBR
addNBRS v nbrs m = foldl' (\m' n -> addNBR v n m') m nbrs

lstToMap :: [(Int, [Int])] -> NBR
lstToMap lst = foldl' (\m (h, lst) -> addNBRS h lst m) M.empty lst

type Visited = S.IntSet

iterate :: Int -> Visited -> NBR -> Visited
iterate cur visited lookup = let
  nbrs = M.findWithDefault [] cur lookup
  non_visited = filter (\v -> not $ S.member v visited) nbrs
  visited' = S.union visited $ S.fromList non_visited
  in
      foldl' (\vst n -> Day12.iterate n vst lookup) visited' non_visited

solve1 :: Text -> Int
solve1 txt = let
  kv = Day12.parse txt
  vst = Day12.iterate 0 S.empty $ lstToMap kv
  in S.size vst

type NotVisited = S.IntSet

groups :: NotVisited -> NBR -> Int
groups nv nbr = grp nv S.empty 0
  where
    grp nv v cnt | S.size nv == 0 = cnt
                 | otherwise = grp nv' visited' cnt'
      where
        (cur:_) = S.elems nv
        visited' = Day12.iterate cur v nbr
        nv' = S.difference nv visited'
        cnt' = cnt + 1

solve2 :: Text -> Int
solve2 txt = let
  kv = Day12.parse txt
  m = lstToMap kv
  nv = foldl' (flip S.insert) S.empty $ M.keysSet m
  in groups nv m
