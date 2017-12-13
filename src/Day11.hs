{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day11 where

import Protolude hiding ((<|>))
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text

data Dir = NW
         | N
         | NE
         | SE
         | S
         | SW
         deriving (Show, Eq)

ts :: [Char] -> Parsec Text () [Char]
ts s = P.try $ string s

dir :: Parsec Text () Dir
dir = do
  d <- ts "nw" <|> ts "ne" <|> ts "n" <|> ts "se" <|> ts "sw" <|> ts "s"
  skipMany $ P.char ','
  spaces
  return $ case d of
    "nw" -> NW
    "n" -> N
    "ne" -> NE
    "se" -> SE
    "s" -> S
    "sw" -> SW

dirs :: Parsec Text () [Dir]
dirs = many1 dir

parseDirections :: Text -> [Dir]
parseDirections txt = 
  case parse  dirs "day11" txt of
    Left err -> traceShow err []
    Right dirs -> dirs

type Coord = (Int, Int)

move :: Coord -> Dir -> Coord
move (x,y) NW = (x - 1, y + 1)
move (x,y) N =  (x + 0, y + 2)
move (x,y) NE = (x + 1, y + 1)
move (x,y) SE = (x + 1, y - 1)
move (x,y) S =  (x + 0, y - 2)
move (x,y) SW = (x - 1, y - 1)

dist :: Coord -> Int
dist (x,y) = let
  x1 = x - 1
  y1 = y - 1
  in
    ((abs x1) + (abs y1)) `div` 2
    

solve1 :: Text -> Int
solve1 txt = let
  dirs = parseDirections txt
  start = (1,1)
  end = foldl' move start dirs
  in dist end

solve2 :: Text -> Int
solve2 txt = let
  dirs = parseDirections txt
  start = (1,1)
  mv :: Coord -> Dir -> Int -> (Coord, Int)
  mv pos d m = (pos', m')
    where
      pos' = move pos d
      m' = maximum [m,  dist pos']
  (_, max) = foldl' (\(pos, m) d -> mv pos d m) (start, 0) dirs
  in max
  
