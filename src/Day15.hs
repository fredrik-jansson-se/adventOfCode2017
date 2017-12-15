{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day15 where

import Protolude

aFactor = 16807
bFactor = 48271
divisor = 2147483647
iters = 5000000

advanceA v = r `seq` r
  where r = (v * aFactor) `rem` divisor

advanceA2 v = let
  r = (v * aFactor) `rem` divisor
  in if r `rem` 4 == 0 then r `seq` r
    else advanceA2 r

advanceB v = r `seq` r 
  where r = (v * bFactor) `rem` divisor

advanceB2 v = let
  r = (v * bFactor) `rem` divisor
  in if r `rem` 8 == 0 then r `seq` r
    else advanceB2 r

replA :: Int -> [Int]
replA init = drop 1 $ reverse $ foldl' (\(h:tl) _ -> (advanceA h):h:tl) [init] [1..iters]

replB :: Int -> [Int]
replB init = drop 1 $ reverse $ foldl' (\(h:tl) _ -> (advanceB h):h:tl) [init] [1..iters]

replA2 :: Int -> [Int]
replA2 init = drop 1 $ reverse $ foldl' (\(h:tl) _ -> (advanceA2 h):h:tl) [init] [1..iters]

replB2 :: Int -> [Int]
replB2 init = drop 1 $ reverse $ foldl' (\(h:tl) _ -> (advanceB2 h):h:tl) [init] [1..iters]

mask :: Int -> Int
mask a = m `seq` m
  where
    m = a .&. 0xffff

solve1 :: Int -> Int -> Int
solve1 a b = let
  z = zip (replA a) (replB b)
  in
    length $ filter (\(a,b) -> Day15.mask a == Day15.mask b) z

solve2 :: Int -> Int -> Int
solve2 a b = let
  z = zip (replA2 a) (replB2 b)
  in
    length $ filter (\(a,b) -> Day15.mask a == Day15.mask b) z
