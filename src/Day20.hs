{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Day20 where

import Protolude
import Data.Monoid
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number

type Vector = (Int, Int, Int)

-- instance Monoid Vector where
--   mempty = (0, 0, 0)
add (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

data Particle = Particle {
  position :: Vector,
  velocity :: Vector,
  acceleration :: Vector
} deriving (Show)

pVector :: Parsec Text () Vector
pVector = do
  char '<'
  x <- int
  char ','
  y <- int
  char ','
  z <- int
  char '>'
  return (x, y, z)

pParticle :: Parsec Text () Particle
pParticle = do
  string "p="
  p <- pVector
  char ','
  spaces
  string "v="
  v <- pVector
  char ','
  spaces
  string "a="
  a <- pVector
  P.optional newline
  return $ Particle p v a

parse :: Text -> [Particle]
parse txt = case P.parse (many1 pParticle) "day20" txt of 
  Left err -> traceShow err []
  Right pts -> pts

distToZero :: Particle -> Int
distToZero Particle { position = pos } = let
  (x,y,z) = pos
  in
    abs x + abs y + abs z

simulate :: Particle -> Particle
simulate part@Particle { position = p, velocity = v, acceleration = a} = let
  v' = v `add` a
  p' = p `add` v'
  in
    part { position = p', velocity = v' }

type PartState = (Bool, Int, Particle)

advance :: PartState -> PartState
advance p@(False, oldDist, part) = p
advance (_, oldDist, part) = let
  part' = simulate part
  newDist = distToZero part'
  in
    if newDist < oldDist
        then (True, newDist, part')
        else (False, oldDist, part)

anyActive :: [PartState] -> Bool
anyActive = any (\(a, _, _) -> a)

run1 :: [PartState] -> [PartState]
run1 p | anyActive p = run1 $ map advance p
       | otherwise   = p

solve1 :: Text -> Int
solve1 txt = let
  pts = Day20.parse txt
  pd = map (\p -> (True, distToZero p, p)) pts
  res = run1 pd
  in
    traceShow (res) 10

