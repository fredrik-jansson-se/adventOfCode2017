{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day18 where

import Protolude
import qualified Base as PBase
import Data.List ((!!))
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

data Tile = Safe
          | Trap
          deriving (Eq)

instance PBase.Show Tile where
--   showsPrec _ Safe = \s -> s ++ "."
  show Safe = "."
  show Trap = "^"

toTile :: Char -> Tile
toTile '.' = Safe
toTile '^' = Trap

left :: [Tile] -> Int -> Tile
left tiles idx = 
  if idx > 0 
    then tiles !! (idx - 1)
    else Safe

right :: [Tile] -> Int -> Tile
right tiles idx = 
  if idx < safeLen
    then tiles !! (idx + 1)
    else Safe
  where
    safeLen = length tiles - 1

center :: [Tile] -> Int -> Tile
center tiles idx = tiles !! idx

-- Its left and center tiles are traps, but its right tile is not.
-- Its center and right tiles are traps, but its left tile is not.
-- Only its left tile is a trap.
-- Only its right tile is a trap.
getTile :: [Tile] -> Int -> Tile
getTile previous idx
  | l == Trap && r == Safe = Trap
  | l == Safe && r == Trap = Trap
  | otherwise              = Safe
  where
    l = left previous idx
    r = right previous idx
          

generateRow previous =
  [getTile previous idx | idx <- [0..l]]
  where
    l = length previous - 1

generateRows :: (Num t, Eq t) => [[Tile]] -> t -> [[Tile]]
generateRows rows 0 = rows
generateRows rows@(p:_) cnt = generateRows rows' cnt'
  where
    new = generateRow p
    rows' = new : rows
    cnt' = cnt - 1

part1 :: IO ()
part1 = do
  start <- (filter (/= '\n') . T.unpack) <$> readFile "day18"
  let floor = generateRows [map toTile start] 39
  let safe = length $ filter (==Safe) $ concat floor 
  let ans = show safe :: Text
  putStrLn $ mappend "day18-1: " ans

part2 :: IO ()
part2 = do
  start <- (filter (/= '\n') . T.unpack) <$> readFile "day18"
  let floor = generateRows [map toTile start] (400000 - 1)
  let safe = length $ filter (==Safe) $ concat floor 
  let ans = show safe :: Text
  putStrLn $ mappend "day18-2: " ans


run :: IO ()
run = do
  part1
  part2

