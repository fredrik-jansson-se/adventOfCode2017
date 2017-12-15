{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day13 where

import Protolude

import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M

-- 0: 3
line :: Parsec Text () (Int, Int)
line = do
  k <- int
  char ':'
  spaces
  val <- int
  newline
  return (k, val)

parse :: Text -> [(Int, Int)]
parse txt = 
  case P.parse (many1 line) "day13" txt of
    Left err -> traceShow err []
    Right xs -> xs

data Layer = Layer {
  layerPos :: Int,
  layerDir :: Int,
  layerBottom :: Int
} deriving (Show)

advance :: Layer -> Layer
advance (Layer pos dir bottom) 
  | pos == 0 && dir == -1 = Layer (pos + 1) 1 bottom
  | pos == bottom && dir == 1 = Layer (pos - 1) (-1) bottom 
  | otherwise = Layer (pos + dir) dir bottom


createLayer :: Int -> Layer
createLayer range = Layer 0 1 (range - 1)

type LM = M.Map Int Layer

advanceScanners :: LM -> LM
advanceScanners = M.map advance

isCaught :: Int -> LM -> Bool
isCaught pos layers = 
  case M.lookup pos layers of
    Nothing -> False
    Just l -> 0 == layerPos l

severity :: LM -> Int -> Int
severity m pos = pos * range
  where
    l = m M.! pos
    range = 1 + layerBottom l

solve1 :: Text -> Int
solve1 txt = let
  dpts = Day13.parse txt :: [(Int, Int)]
  layers = map createLayer $ M.fromList dpts
  (max, _) = M.findMax layers
  (_, res) = foldl' f (layers, []) [0..max]
  f (layers, caught) pos | isCaught pos layers = (advanceScanners layers, (pos:caught))
                         | otherwise = (advanceScanners layers, caught)
  sevs = map (severity layers) res
  in sum sevs

delay :: Int -> LM -> LM
delay cnt layers | cnt == 0 = layers
                 | otherwise = delay (cnt - 1) $ advanceScanners layers

icc [] _ = False
icc (x:xs) layers = 
  if isCaught x layers 
    then True
    else icc xs (advanceScanners layers)

try max delay layers =
  if icc [0..max] layers
    then Day13.try max (delay + 1) (advanceScanners layers)
    else delay
  

solve2 :: Text -> Int
solve2 txt = let
  dpts = Day13.parse txt :: [(Int, Int)]
  layers = map createLayer $ M.fromList dpts
  (max, _) = M.findMax layers
  in Day13.try max 0 layers

