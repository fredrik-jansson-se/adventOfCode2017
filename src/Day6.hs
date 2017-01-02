module Day6 where

import Data.List (maximumBy, minimumBy)
import qualified Data.Map.Strict as Map

type CM = Map.Map Char Int
type CMM = Map.Map Int CM

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zipWith (\idx a -> (idx, a)) [0..]

updateCount :: Char -> CM -> CM
updateCount c m = Map.insert c newCnt m
  where
    newCnt = 1 + Map.findWithDefault 0 c m

updateIndex :: CMM -> (Int, Char) -> CMM
updateIndex m (idx, c) = Map.insert idx newCM m
  where
    cm = Map.findWithDefault Map.empty idx m
    newCM = updateCount c cm

updateIndices :: CMM -> [(Int, Char)] -> CMM
updateIndices = foldl updateIndex

getMax :: CM -> Char
getMax m = char
  where
    (char, _) = maximumBy (\(_, cnt1) (_, cnt2) -> compare cnt1 cnt2) $ Map.toList m

getMin :: CM -> Char
getMin m = char
  where
    (char, _) = minimumBy (\(_, cnt1) (_, cnt2) -> compare cnt1 cnt2) $ Map.toList m

part1 :: IO ()
part1 = do
  input <- readFile "day6"
  let zips = map zipWithIndex $ lines input
  let cntx = foldl updateIndices Map.empty zips :: CMM
  let maxes = Map.toList $ Map.map getMax cntx
  let ans = map (\(_,c) -> c) maxes :: String
  putStrLn $ "day6-1: " ++ ans


part2 :: IO ()
part2 = do
  input <- readFile "day6"
  -- let input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
  let zips = map zipWithIndex $ lines input
  let cntx = foldl updateIndices Map.empty zips :: CMM
  let maxes = Map.toList $ Map.map getMin cntx
  let ans = map (\(_,c) -> c) maxes :: String
  putStrLn $ "day6-2: " ++ ans

run :: IO ()
run = do
  part1
  part2
