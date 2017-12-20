{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day14 where

import Protolude

import qualified Day10 as D10
import qualified Data.Text as T
import TextShow (showt)
-- import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

toMap :: Char -> [Int]
toMap '0' = [0, 0, 0, 0]
toMap '1' = [0, 0, 0, 1]
toMap '2' = [0, 0, 1, 0]
toMap '3' = [0, 0, 1, 1]
toMap '4' = [0, 1, 0, 0]
toMap '5' = [0, 1, 0, 1]
toMap '6' = [0, 1, 1, 0]
toMap '7' = [0, 1, 1, 1]
toMap '8' = [1, 0, 0, 0]
toMap '9' = [1, 0, 0, 1]
toMap 'a' = [1, 0, 1, 0]
toMap 'b' = [1, 0, 1, 1]
toMap 'c' = [1, 1, 0, 0]
toMap 'd' = [1, 1, 0, 1]
toMap 'e' = [1, 1, 1, 0]
toMap 'f' = [1, 1, 1, 1]

toMapRow :: Text -> [Int]
toMapRow txt = concat $ map toMap $ T.unpack txt

type Pos = (Int, Int)
type LU = S.Set Pos

rowToLu :: LU -> (Int, [Int]) -> LU
rowToLu lu (row, bits) = foldl' (\m (v,col) -> if v == 1 then S.insert (row, col) m else m) lu (zip bits [0..])

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

isNeighbor :: Pos -> Pos -> Bool
isNeighbor (r1, c1) (r2, c2) = 1 == dr + dc
  where
    dr = abs $ r2 - r1
    dc = abs $ c2 - c1

removeNeighbors :: LU -> Pos -> LU
removeNeighbors lu p = let
  nbrs = S.filter (isNeighbor p) lu
  lu' = S.delete p lu
  lu'' = foldl' removeNeighbors lu' nbrs
  in
    lu''

countNbrs :: LU -> Int -> Int
countNbrs lu cnt | S.size lu == 0 = cnt
countNbrs lu cnt = let
  lu' = removeNeighbors lu $ S.elemAt 0 lu
  cnt' = cnt + 1
  in 
    countNbrs lu' cnt'

solve2 :: Text -> Int
solve2 key = let
  inp = inputs key
  hashs = map (D10.hash 256) inp
  mm = map toMapRow hashs :: [[Int]]
  lu = foldl' rowToLu S.empty $ zip [0..] mm
  cnt = countNbrs lu 0
  in 
    cnt
