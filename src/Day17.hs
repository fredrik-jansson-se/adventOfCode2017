{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day17 where

import Protolude

insAt lst pos val = let
  (h,t) = splitAt pos lst
  in
    h++[val]++t

run1 :: Int -> Int -> [Int] -> Int -> (Int, [Int])
run1 step pos lst val = let
  next_pos = (pos + step) `rem` (length lst) + 1
  in
    (next_pos, insAt lst next_pos val)

solve1 :: Int -> Int
solve1 step = let
  i = [0]
  p0 = 0
  (_, lst) = foldl' (\(pos,lst) val -> run1 step pos lst val) (p0, i) [1..2017]
  (_:b:_) = dropWhile (/=2017) lst
  in b

step2 step pos vAt1 val size = let
  pos' = (pos + step) `rem` size
  insertPos = pos' + 1
  vAt1' = if insertPos == 1 then val else vAt1
  in
    (insertPos `seq` insertPos, vAt1' `seq` vAt1')

run2 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
run2 step pos vAt1 val size cnt 
  | cnt == 0 = vAt1
  | otherwise = let
    (pos', vAt1') = step2 step pos vAt1 val size
    val' = val+1
    size' = size+1
    cnt' = cnt - 1
    in 
      run2 step pos' vAt1' val' size' cnt'
         

solve2 :: Int -> Int
solve2 step = run2 step 0 0 1 1 50000000
