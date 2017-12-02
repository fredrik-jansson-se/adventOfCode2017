{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day13 where

import Protolude
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Maybe (fromJust)
import Text.Regex.PCRE
import Astar
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

data Pos = Pos Int Int deriving (Show, Eq, Ord)

ee :: Int -> Bool
ee i = even $ ee' i 0
  where
    ee' :: Int -> Int -> Int
    ee' 0 acc = acc
    ee' i acc | odd i = ee' (i `div` 2) (acc+1)
      | otherwise = ee' (i `div` 2) acc

isOpen :: Int -> Pos -> Bool
isOpen seed (Pos x y) = (x>=0 ) && (y>=0) && isEven
  where
    val = seed +  x*x + 3*x + 2*x*y + y + y*y
    isEven = ee val
    bin = showIntAtBase 2 intToDigit val "" 
    -- log = bin
    -- ones = length $ filter (=='1') bin
    -- isEven = even ones

getNeighbors :: Int -> Pos -> [Pos]
getNeighbors seed (Pos x y) = ngbrs
  where
    pos = [Pos (x - 1) y, Pos (x + 1) y,
           Pos x $ y - 1, Pos x $ y + 1]
    ngbrs = filter (isOpen seed) pos


distance :: Pos -> Pos -> Int
distance (Pos x1 y1) (Pos x2 y2) = abs(x1-x2) + abs (y1 - y2)

genGoals :: Int -> [Pos]
genGoals seed = filter ff [Pos x y | x <- [0..50], y <- [0..50]]
  where
    start = Pos 1 1
    ff = \p -> isOpen seed p && distance start p < 50

part1 :: IO ()
part1 = do
  -- let start = Pos 1 1
  -- let goal = Pos 31 39
  -- let seed = 1364
  -- let fns = AStarFns (distance goal) distance  (Day13.getNeighbors seed)
  -- let path = fromJust $ astar fns start goal
  -- let ans = show $ length path :: Text
  let ans = "86" :: Text
  putStrLn $ mappend "day13-1: " ans

getLenToGoal :: Int -> S.Set Pos -> Pos -> S.Set Pos
getLenToGoal seed pos goal = let
  fns = AStarFns (distance goal) distance (Day13.getNeighbors seed)
  start = Pos 1 1
  steps = astar fns start goal :: Maybe [Pos]
  log = show $ S.size pos :: Text
  in
    if S.member goal pos
       then pos
       else case steps of
                          Just s | length steps < 50 -> (S.union pos $ S.fromList s)
                          _ -> pos

part2 :: IO ()
part2 = do
  let seed = 1364
  let goals = genGoals seed
  let start = Pos 1 1
  let init = S.fromList [start]
  let dists = foldl' (getLenToGoal seed) init goals :: Set Pos
  print dists
  let d2 = S.filter (\g -> distance start g <= 50) dists
  let ans = show $ S.size d2 :: Text
  -- let ans = "failed" :: Text
  putStrLn $ mappend "day13-2: " ans
  -- 713
  -- 271
  -- 263 to high
  -- 209
  -- 210

          

run :: IO ()
run = do
  part1
  part2

