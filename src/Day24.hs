{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day24 where

import Protolude
import Data.List (partition)
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number

type Bridge = (Int, Int)

valid :: Bridge -> Bridge -> Bool
valid (a, b) (c, d) = b == c

strength :: Bridge -> Int
strength (a, b) = a + b

totalStrength :: [Bridge] -> Int
totalStrength = foldl' (\s b -> s + strength b) 0 

isZero (a, b) = a == 0 || b == 0

pairsValid :: [Bridge] -> Bool
pairsValid (_:[]) = True
pairsValid (a:b:xs) = if valid a b then pairsValid (b:xs) else False

allValid :: [Bridge] -> Bool
allValid b@(x:xs) 
  | isZero x = traceShow b $ pairsValid b
  | otherwise = False

initValid :: [Bridge] -> Bool
initValid (a:_) = isZero a

takeWhileValid :: [Bridge] -> (Int, Int)
takeWhileValid = tw 0 0
  where
    tw str len [] = (str, len)
    tw str len (a:b:xs) = tw str' len' cs
       where 
        str' = str `seq` str + strength a
        len' = len `seq` len + 1
        bf = swap b
        cs = if valid a b then (b:xs) else if valid a bf then (bf:xs) else []

parseBridge :: Parsec Text () Bridge
parseBridge = do
  a <- int
  char '/'
  b <- int
  P.optional newline
  return (a, b)

parseBridges :: Text -> [Bridge]
parseBridges txt = case P.parse (many1 parseBridge) "day24" txt of
  Right res -> res
  Left err -> traceShow err []

fixInit :: Bridge -> Bridge
fixInit i@(0,_) = i
fixInit i = swap i

validNext (_,b) = foldl' (\acc v@(a',b') -> if b == a' || b == b' then (v:acc) else acc) 

solve1 txt = let
  bridges = parseBridges txt
  (inits, rest) = partition isZero bridges
  flipped = map swap bridges
  allBridges = bridges 
  perms = permutations allBridges
  valids = filter initValid perms
  brs = map takeWhileValid valids
  in
    -- traceShow (inits, rest) $ maximum $ map fst brs
    traceShow (inits, length rest) $ 31
