{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day20 where

import Protolude
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion (fromByteString)
import Data.Maybe (fromJust)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

data Range = 
  Range Int Int
  deriving (Eq, Show)

instance Ord Range where
  (Range _ h) <= (Range l _) = h <= l
  
overlapping :: Range -> Range -> Bool
overlapping (Range l1 h1) (Range l2 h2) = 
  (l2 <= l1 && l1 <= h2) ||
    (l1 <= l2 && l2 <= h1) ||
      (l2 - h1 == 1)

inRange :: Int -> Range -> Bool
inRange i (Range l h) = l <= i && i <= h

parse :: ByteString -> Range
parse r = Range l h
  where
    regex = "(\\d+)\\-(\\d+)" :: ByteString
    matches = r =~ regex :: AllTextSubmatches [] ByteString
    (_:a:b:[]) = getAllTextSubmatches matches :: [ByteString]
    l = fromJust $ fromByteString a 
    h = fromJust $ fromByteString b

combine :: Range -> Range -> Range
combine (Range l1 h1) (Range l2 h2) = 
  Range l h
    where 
      l = min l1 l2
      h = max h1 h2

combine2 :: [Range] -> [Range]
combine2 rngs = c2 rngs []
  where
    c2 :: [Range] -> [Range] -> [Range]
    c2 (r:[]) acc = reverse $ r:acc
    c2 (r:r':rs) acc 
      | overlapping r r' = c2 (rr':rs) acc
      | otherwise = c2 (r':rs) (r:acc)
      where rr' = combine r r'

inRanges :: [Range] -> Int -> Bool
inRanges [] _ = False
inRanges (r:rs) i 
  | inRange i r = True
  | otherwise = inRanges rs i


size :: Range -> Int
size (Range l h) = h - l + 1

part1 :: IO ()
part1 = do
  -- let d = "5-8\n0-2\n4-7" :: ByteString
  -- d <- C8.readFile "day20"
  -- let ranges = sort $ map parse $ C8.lines d :: [Range]
  -- let combined = combine2 ranges
  -- putStrLn $ (show combined :: Text)
  -- let lowest = find (\i -> not $ inRanges combined i) [0..]
  -- let ans = show lowest :: Text
  let ans = "31053880" :: Text
  putStrLn $ mappend "day20-1: " ans

summer :: [Range] -> Int -> Int
summer [] acc = acc
summer (a:[]) acc = acc
summer (a:b:rs) acc = summer (b:rs) newAcc
  where
    (Range _ h) = a
    (Range l _) = b
    newAcc = acc + l - h - 1

part2 :: IO ()
part2 = do
  -- d <- C8.readFile "day20"
  -- let ranges = sort $ map parse $ C8.lines d :: [Range]
  -- let combined = combine2 $ sort $ combine2 $ sort $ combine2 $ sort $ combine2 ranges
  -- putStrLn (show combined :: Text)
  -- let ans = show (summer combined 0) :: Text
  let ans  = "117" :: Text
  putStrLn $ mappend "day20-2: " ans


run :: IO ()
run = do
  part1
  part2

