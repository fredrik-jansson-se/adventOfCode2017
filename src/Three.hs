module Three where

import Data.List (sort)
import Data.List.Split (splitOn)
import Debug.Trace

parseLine :: String -> (Int, Int, Int)
parseLine str = 
  let tokens = filter (/="") $ splitOn " " str
      st = show tokens
      (a:b:c:[]) = map read tokens
  in
    (a,b,c)

convert :: [(Int, Int, Int)] -> [Int]
convert tuples = reverse l1 ++ reverse l2 ++ reverse l3
  where
    fn (l1,l2,l3) (a,b,c) = (a:l1, b:l2, c:l3)
    (l1,l2,l3) = foldl fn ([], [], []) tuples

convertBack :: [Int] -> [(Int, Int, Int)]
convertBack [] = []
convertBack (a:b:c:xs) = (a', b', c') : convertBack xs
  where
    [a', b', c'] = sort (a:b:c:[])

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a,b,c) = a + b > c

run :: IO String
run = do
  input <- readFile "day3/input" 
  -- let input = "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603"
  -- let input = "5 4 3\n3 4 5\n1 5 2\n"
  let triples = map parseLine $ lines input
  -- putStrLn $ show triples
  let converted = convert triples
  -- putStrLn $ "converted = " ++ (show $ converted)
  let colTrips = convertBack converted
  -- putStrLn $ "colTrips = " ++ (show $ colTrips)
  let tris = filter isTriangle colTrips
  return $ "Three = " ++ (show $ length tris)
