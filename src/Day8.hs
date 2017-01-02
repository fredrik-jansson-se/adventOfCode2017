module Day8 where

import Debug.Trace
import Data.Array
import Data.List (find)
import Data.Maybe
import Text.Regex.TDFA

type Screen = Array (Int, Int) Bool

parseLine :: String -> (Screen -> Screen)
parseLine s = fn
  where
    rM = "rect ([0-9]+)x([0-9]+)"
    rotRM = "rotate row y=([0-9]+) by ([0-9]+)"
    rotCM = "rotate column x=([0-9]+) by ([0-9]+)"
    fn = if s =~ rM
            then let (r,c) = getInts rM in rect r c
             else if s =~ rotRM 
                     then let (r,d) = getInts rotRM in rotateRow r d
                              else let (c,d) = getInts rotCM in rotateColumn c d
    getInts :: String -> (Int, Int)
    getInts r = (a,b)
      where
        (_, _, _, res) = s =~ r :: (String, String, String, [String])
        (a:b:_) = map read res

width :: Int
width = 50
maxC = width - 1

height :: Int
height = 6
maxR = height - 1

createArray :: Screen
createArray = listArray ((0,0), (maxR, maxC)) $ repeat False

showB :: Bool -> Char
showB True = '#'
showB False = ' '

showA :: Screen -> String
showA s = unlines rows
  where
    rows :: [String]
    rows = [[showB $ s ! (r,c) | c <- [0..maxC]] | r <- [0..maxR]]

rect :: Int -> Int -> Screen -> Screen
rect w h s = s // vals
  where
    vals = [((r,c), True) | r <- [0..(h-1)], c <- [0..(w-1)]]
    log = "rect " ++ show [w,h]

rotate :: Int -> [a] -> [a]
rotate d xs = reverse $ b++a
  where
    (a,b) = splitAt d $ reverse xs

getColumn :: Int -> Screen -> [Bool]
getColumn c scr = [scr ! (r,c) | r <- [0..maxR]]

setColumn :: Int -> Screen -> [Bool] -> Screen
setColumn c scr xs = scr // [((r,c), xs !! r) | r <- [0..maxR]]

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn c d scr = new
  where
    vals = getColumn c scr
    rotated = rotate d vals
    new = setColumn c scr rotated

getRow :: Int -> Screen -> [Bool]
getRow r scr = [scr ! (r,c) | c <- [0..maxC]]

setRow :: Int -> Screen -> [Bool] -> Screen
setRow r scr xs = scr // [((r,c), xs !! c) | c <- [0..maxC]]

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow r d scr = new
  where
    vals = getRow r scr
    rotated = rotate d vals
    new = setRow r scr rotated

count :: Screen -> Int
count = foldl (\s v -> if v then s+1 else s) 0 

part1 :: IO ()
part1 = do
  input <- readFile "day8"
  let fns = map parseLine $ lines input
  let final = foldl (\s f -> f s) createArray fns
  let ans = show $ count final
  putStrLn $ "day8-1: " ++ ans

part2 :: IO ()
part2 = do
  input <- readFile "day8"
  let fns = map parseLine $ lines input
  let final = foldl (\s f -> f s) createArray fns
  let ans = "\n" ++ showA final
  -- CFLELOYFCS
  putStrLn $ "day8-2: " ++ ans

run :: IO ()
run = do
  part1
  part2

