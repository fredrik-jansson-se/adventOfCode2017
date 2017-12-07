{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day5 where

import Protolude
import Common
import qualified Data.Text as T
import qualified Data.Map as M

type MM = M.Map Int Int

step1 :: MM -> Int -> (MM, Int)
step1 m idx = let
  jmp = M.findWithDefault 0 idx m
  idx' = idx + jmp
  jmp' = jmp + 1
  m' = M.insert idx jmp' m
  in
    (m', idx')

run1 :: MM -> Int -> Int -> Int
run1 m idx steps = let 
  (m', idx') = step1 m idx
  in case M.lookup idx m of
    Nothing -> steps
    Just _ -> run1 m' idx' $ steps + 1

toMM :: [Int] -> MM
toMM ints = M.fromList $ zip [0..] ints

solve1 :: Text -> Int
solve1 txt = let
  lines = T.lines txt
  ints = map (parseInt 0) lines
  m = toMM ints
  in run1 m 0 0

step2 :: MM -> Int -> (MM, Int)
step2 m idx = let
  jmp = M.findWithDefault 0 idx m
  idx' = idx + jmp
  jmp' = if jmp > 2 then jmp - 1 else jmp + 1
  m' = M.insert idx jmp' m
  in
    (m', idx')

run2 :: MM -> Int -> Int -> Int
run2 m idx steps = let 
  (m', idx') = step2 m idx
  in case M.lookup idx m of
    Nothing -> steps
    Just _ -> run2 m' idx' $ steps + 1

-- 2 3 2 3 -1

solve2 :: Text -> Int
solve2 txt  = let
  lines = T.lines txt
  ints = map (parseInt 0) lines
  m = toMM ints
  in run2 m 0 0
