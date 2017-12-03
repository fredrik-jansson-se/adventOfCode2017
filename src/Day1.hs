{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day1 where

import Protolude

import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List ((!!))
import Common

count :: [Int] -> Int -> Int
count (_:[]) sum = sum
count (a:b:tl) sum 
  | a == b = count (a:tl) (sum + a)
  | otherwise = count (b:tl) sum


solve :: Text -> Int
solve txt = let 
  i = toInts txt
  h = fromJust $ head i
  ii = i ++ [h]
  in
   count ii 0

count2 :: [Int] -> Int -> Int -> Int -> Int
count2 _ _ 0 sum = sum
count2 (a:tl) add len sum = let
  b = tl !! add
  nlen = len - 1
  nsum = sum + a
  in
  -- traceShow (a, b, tl) $
    if a == b
        then count2 tl add nlen nsum
        else count2 tl add nlen sum
  

solve2 :: Text -> Int
solve2 txt = let
  i = toInts txt
  l = length i
  add = l `div` 2 - 1
  ii = i ++ i
  in 
    count2 ii add l 0
    

