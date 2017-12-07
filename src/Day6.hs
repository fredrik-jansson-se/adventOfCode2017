{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day6 where
import Protolude

import Common
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

type MM = M.Map Int Int
type MStore = S.Set MM

toMM :: [Int] -> MM
toMM = M.fromList . zip [0..]

maxPos :: MM -> (Int, Int)
maxPos = M.foldlWithKey (\(k,v) k' v' -> if v' > v then (k', v') else (k,v)) (0,0)

update :: MM -> MM
update m = let
  (idx, max) = maxPos m
  s = M.size m
  (div, rem) = max `divMod` s
  startIdx = (idx + 1) `mod` s
  endIdx = (idx + rem) `mod` s
  extra = if rem == 0 then []
      else if startIdx > endIdx
      then [0..endIdx]++[startIdx..s-1] 
      else [startIdx..endIdx]
  m' = M.insert idx 0 m
  m'' = M.mapWithKey fn m'
  fn k val | k `elem` extra = 1 + val + div
           | otherwise = val + div
  in
    m''

run1 :: MStore -> MM -> Int -> (Int, MM)
run1 s m cnt | S.member m s = (cnt, m)
             | otherwise = run1 (S.insert m s) (update m) (cnt+1)

solve1 :: Text -> Int
solve1 txt = let
  ints = map (parseInt 0) $ T.words txt
  m = toMM ints
  s = S.empty :: MStore
  in
    fst $ run1 s m 0

solve2 :: Text -> Int
solve2 txt = let
  ints = map (parseInt 0) $ T.words txt
  m = toMM ints
  s = S.empty :: MStore
  (cnt1, m1) = run1 s m 0
  in
    fst $ run1 s m1 0
