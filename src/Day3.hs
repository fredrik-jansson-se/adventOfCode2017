{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day3 where

import Protolude

import qualified Data.Map.Strict as Map

-- 1
-- 9 - 2 + 1 = 8
-- 25 - 10 + 1 = 16
-- 49 - 26 + 1 = 24

-- -3 37  36  35  34  33  32  31
-- -2 38  17  16  15  14  13  30
-- -1 39  18   5   4   3  12  29
--  0 40  19   6   1   2  11  28
--  1 41  20   7   8   9  10  27
--  2 42  21  22  23  24  25  26
--  3 43  44  45  46  47  48  49
--    -3  -2  -1   0   1   2   3

findSquare :: Int -> Int
findSquare v = fs v 1
  where
    fs v s | s*s >= v = s
           | otherwise = fs v $ s + 2

seCoord :: Int -> (Int, Int)
seCoord s = (v, -v) 
  where
    v = s `div` 2

swCoord :: Int -> (Int, Int)
swCoord s = (x - s + 1, y)
  where
    (x,y) = seCoord s

nwCoord :: Int -> (Int, Int)
nwCoord s = (-x, -y)
  where
    (x,y) = seCoord s

neCoord :: Int -> (Int, Int)
neCoord s = (x, y + s - 1)
  where
    (x,y) = seCoord s


getCoord val = let
 s = findSquare val 
 s2 = s * s
 dv = s2 - val + 1
 cfn | dv > 3 * (s-1) = (neCoord, (0, -1), dv - 3 * (s - 1) - 1)
     | dv > 2 * (s-1) = (nwCoord, (1, 0), dv - 2 * (s - 1) - 1)
     | dv > s - 1     = (swCoord, (0, 1), dv - s)
     | otherwise      = (seCoord, (-1, 0), dv - 1)
 (fn, (dx,dy), cnt) = cfn
 startCoord = fn s
 coord 0 c = c
 coord cnt c = coord (cnt - 1) newCoord
  where
    (x,y) = c
    newCoord = (x+dx, y+dy)
 in
  coord cnt startCoord

solve1 val = let
  (x,y) = getCoord val
  in
    abs x + abs y

turnLeft (1,0) = (0,1)
turnLeft (0,1) = (-1, 0)
turnLeft (-1, 0) = (0, -1)
turnLeft (0, -1) = (1, 0)

type Coord = (Int, Int)
type MM = Map.Map Coord Int

occupied :: MM -> Coord -> Bool
occupied m c = Map.member c m

add :: Coord -> Coord -> Coord
add (x,y) (dx, dy) = (x + dx, y + dy)

neighbors :: Coord -> [Coord]
neighbors (x',y') = [(x,y)| x<-[x'-1..x'+1], y<-[y'-1..y'+1], (x, y) /= (x', y')]

move :: MM -> Coord -> Coord -> (Coord, Coord)
move m cur dir = let
  leftDir = turnLeft dir
  leftCoord = cur `add` leftDir
  aheadCoord = cur `add` dir
  in
  if occupied m leftCoord
    then (aheadCoord, dir)
    else (leftCoord, leftDir)
        

neighborSum :: MM -> Coord -> Int
neighborSum d c = let
  nbrs = neighbors c
  sums = map (\n -> Map.findWithDefault 0 n d) nbrs
  in
    sum sums

s2 :: Int -> MM -> Coord -> Coord -> Int
s2 maxSum m cur dir = let
  (next, nextDir) = move m cur dir
  m' = Map.insert next (neighborSum m next) m
  sum = Map.findWithDefault 0 cur m'
  in
    if sum > maxSum then sum
    else s2 maxSum m' next nextDir

solve2 :: Int -> Int
solve2 maxSum = let
  startCoord = (0,0)
  startDir = (0, -1)
  startMap = Map.singleton startCoord 1
  sum = s2 maxSum startMap startCoord startDir
  in 
    sum


