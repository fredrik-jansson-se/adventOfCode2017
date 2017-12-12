{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day10 where

import Protolude
import Common
import qualified Data.Text as T
import Data.List (length, splitAt)
import Numeric (showHex)

advance :: Int -> Int -> Int -> [Int] -> (Int, [Int])
advance pos len skip lst = let
  (h, t) = splitAt pos lst
  (h', t') = splitAt len $ t ++ h
  hr = reverse h'
  (h'', t'') = splitAt (length t) $ hr ++ t'
  nextPos = (pos + len + skip) `rem` (length lst)
  in
    (nextPos, t'' ++ h'')

run1 :: [Int] -> [Int] -> Int -> Int -> [Int]
run1 lst [] _ _ = lst
run1 lst (l:ls) pos skip = run1 lst' ls pos' skip'
   where
    (pos', lst') = advance pos l skip lst
    skip' = skip + 1

solve1 :: Int -> Text -> Int
solve1 len txt = let
  lts = csv txt
  ring = [0..(len-1)]
  pos = 0
  skip = 0
  (a:b:_) = run1 ring lts pos skip
  in  
    a*b


asciiLengths :: Text -> [Int]
asciiLengths txt = al (T.unpack txt) []
  where
    al [] acc = reverse acc
    al (c:xs) acc = al xs (i:acc)
      where
        i = ord c

parseLengths :: Text -> [Int]
parseLengths txt = asciiLengths txt ++ endSeq
  where
    endSeq = [17, 31, 73, 47, 23]

run2 :: [Int] -> [Int] -> Int -> Int -> [Int]
run2 lst lts pos skip = run1 lst lts' pos skip
  where
    lts' = concat $ replicate 64 lts

group :: Int -> [a] -> [[a]]
group size lst = grp lst []
  where
    grp :: [a] -> [[a]] -> [[a]]
    grp lst acc | length lst <= size = reverse $ lst:acc
                | otherwise = grp t (h:acc)
                    where
                      (h,t) = splitAt size lst

denseHash :: [Int] -> [Int]
denseHash sparse = let
  grp = Day10.group 16 sparse
  xorList = foldl' xor 0 
  in
    map xorList grp

sh :: Int -> [Char]
sh v = if length h < 2 
          then '0':h
          else h
     where
      h = showHex v ""
  

solve2 len txt = let
  line = case head $ T.lines txt of
          Nothing -> ""
          Just h -> h
  lts = parseLengths line
  ring = [0..(len-1)]
  pos = 0
  skip = 0
  sparseHash = run2 ring lts pos skip
  dh = denseHash sparseHash
  hash = concat $ map sh dh 
  in
    T.pack hash
  
