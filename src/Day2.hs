{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day2 where

import Protolude
import qualified Data.Text as T
import Data.Text.Read

toInt :: Text -> Int
toInt txt = 
  case decimal txt of
    Right (i, _) -> i
    _ -> 0

toInts :: [Text] -> [Int]
toInts = map $ toInt 

maxMin :: [Int] -> (Int, Int)
maxMin (a:tl) = mm a a tl
  where
    mm min max [] = (min, max)
    mm min max (x:xs) = let
      nmax = if x > max then x else max
      nmin = if x < min then x else min
      in
        mm nmin nmax xs

solve1 :: Text -> Int
solve1 txt = let
  l = T.lines txt
  li = map T.words l
  ints = map toInts li
  mm = map maxMin ints
  diffs = map (\(a,b) -> b - a) mm
  in
    sum diffs

hdivs :: Int -> [Int] -> Int -> Int
hdivs _ [] sum = sum
hdivs a (x:xs) sum 
  | x `rem` a == 0 = hdivs a xs (sum + x `div` a)
  | otherwise = hdivs a xs sum

divs :: Int -> [Int] -> Int
divs sum [] = sum
divs sum (x:xs) = let
  d = hdivs x xs 0
  in
    divs (sum+d) xs

solve2 :: Text -> Int
solve2 txt = let
  l = T.lines txt
  li = map T.words l
  ints = map (sort . toInts) li
  d = map (divs 0) ints
  in
    sum d
