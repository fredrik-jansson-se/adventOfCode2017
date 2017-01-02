{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Four where

import Data.List (intersperse, isInfixOf, nub, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Debug.Trace
import Text.Regex.TDFA

newtype Checksum = Checksum String deriving (Eq, Show)

newtype SectorID = SectorID Int deriving (Show)

parseLine :: String -> ([String], SectorID, Checksum)
parseLine lines =
  let items = reverse $ splitOn "-" lines
      names = tail items
      r = "([0-9]+)\\[([a-z]+)\\]" :: String
      match = (head items) =~ r :: MatchResult String
      matches = mrSubList match
      sectorID = SectorID $ read $ head matches
      checksum = Checksum $ head $ tail matches
  in
    (reverse names, sectorID, checksum)

counts :: String -> Map.Map Char Int
counts = foldl f Map.empty
  where 
    f :: Map.Map Char Int -> Char -> Map.Map Char Int
    f m c = Map.insert c (1+cnt) m
      where
        cnt = Map.findWithDefault 0 c m

verifyChecksum :: [String] -> Checksum -> Bool
verifyChecksum xs (Checksum cs) =
  s5 == cs
    where
      str = foldl (++) "" xs
      cnt = counts str
      sorted = sortBy sortFn (nub str)
      sortFn c1 c2 = case cnt_comp of 
                       EQ -> compare c2 c1
                       otherwise -> cnt_comp
                     where
                       cnt1 = Map.lookup c1 cnt
                       cnt2 = Map.lookup c2 cnt
                       cnt_comp = compare cnt1 cnt2
      s5 = take 5 $ reverse sorted

decryptString :: SectorID -> String -> String
decryptString (SectorID sectID) enc =
  dec
    where
      a = fromEnum 'a'
      z = fromEnum 'z'
      ens :: [Int]
      ens = (map fromEnum enc)
      decryptChar :: Int -> Char
      decryptChar e = d
        where
          i = a + (e - a + sectID) `mod` (1 + z-a)
          d = toEnum i :: Char
      dec :: String
      dec = map decryptChar ens

makeString :: [String] -> String
makeString = concat . (intersperse " ")
                  
part1 = do
  input <- readFile "day4"
  let parsedLines = map parseLine $ lines input
  let okLines = filter (\(s, _, cs) -> verifyChecksum s cs) parsedLines
  let res = sum $ map (\(_, (SectorID a), _) -> a) okLines
  putStrLn $ "day4 - 1: " ++ show res
  -- correct = 278221

part2 = do
  input <- readFile "day4"
  let parsedLines = map parseLine $ lines input
  let okLines = filter (\(s, _, cs) -> verifyChecksum s cs) parsedLines
  let decrypted = map (\(xs, sid, _) -> (sid, makeString $ map (decryptString sid) xs)) okLines
  let magnetics = filter (\(_, xs) -> isInfixOf "north" xs) decrypted
  putStrLn $ show magnetics

run :: IO ()
run = do
  part1
  part2
