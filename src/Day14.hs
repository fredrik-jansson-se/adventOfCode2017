{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day14 where

import Protolude
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import Data.List (isInfixOf, tail)
import qualified Data.String as S
import qualified Data.Map as M

type HMap = M.Map Int S.String

data HashFn = HashFn { runHash :: HMap -> Int -> (HMap, S.String) }

hash :: ByteString -> HMap -> Int -> (HMap, S.String)
hash salt lu idx =
  case M.lookup idx lu of
    Just h -> (lu, h)
    _ -> (newLu, seh)
  where
    sidx = C8.pack $ show idx
    c = mappend salt sidx
    seh = C8.unpack $ B16.encode $ MD5.hash $ c
    newLu = M.insert idx seh lu

superHash :: ByteString -> HMap -> Int -> (HMap, S.String)
superHash salt lu idx = 
  case M.lookup idx lu of
    Just h -> (lu, h)
    _ -> (newLu, seh)
  where
    sidx = C8.pack $ show idx
    c = mappend salt sidx
    h = B16.encode $ MD5.hash c
    hfn :: ByteString -> ByteString
    hfn a = B16.encode $ MD5.hash a
    eh = foldr (\_ a -> hfn a) h [1..2016]
    seh = C8.unpack eh
    newLu = M.insert idx seh lu


hasTriplet :: S.String -> Maybe Char
hasTriplet h@(a:b:c:_)
  | a == b && b == c = Just a
  | otherwise = hasTriplet $ tail h
hasTriplet h | length h < 3 = Nothing

hasQuintet :: Char -> S.String -> Bool
hasQuintet c str = cc `isInfixOf` str
  where
    cc = replicate 5 c

findTriplet :: HashFn -> HMap -> Int -> (HMap, Int, Char)
findTriplet hfn lu idx = 
  case hasTriplet h of
    Just c -> (newLu, idx, c)
    _ -> findTriplet hfn newLu $ idx + 1
  where
    (newLu, h) = runHash hfn lu idx

findQuintet :: HashFn -> HMap -> Char -> Int -> Int -> (HMap, Maybe Int)
findQuintet hfn lu c max startIdx  = fq lu startIdx
  where
    fq :: HMap -> Int -> (HMap, Maybe Int)
    fq lu idx 
      | max == idx = (lu, Nothing)
      | hasQuintet c h = trace (show idx :: Text) (newLu, Just startIdx)
      | otherwise = fq newLu $ idx + 1
      where
        (newLu, h) = runHash hfn lu idx

findKey :: HashFn -> HMap -> Int -> (HMap, Maybe Int)
findKey hfn lu startIdx = do
  let (newLu1, idx, c) = findTriplet hfn lu startIdx
  let log = "idx = " ++ show idx ++ " char = " ++ show c
  let (newLu2, fq) = findQuintet hfn newLu1 c (idx+1001) (idx+1)
  case fq of
    Just _ -> (newLu2, return idx)
    _ -> (newLu2, Nothing)

findKeys :: HashFn -> [Int]
findKeys hfn = fk M.empty 0 [] 
  where
    fk _ _ keys | length keys == 64 = keys
    fk lu idx keys = case fkres of
                       Just i -> fk newLu (i + 1) (i:keys)
                       Nothing -> fk newLu (idx + 1) keys 
            where
              (newLu, fkres) = findKey hfn lu idx

part1 :: IO ()
part1 = do
  let salt = C8.pack "qzyelonm"
  let hashfn = HashFn $ hash salt
  -- let ans = show $ head $ findKeys hashfn :: Text
  let ans = "15168" :: Text
  putStrLn $ mappend "day14-1: " ans

part2 :: IO ()
part2 = do
  -- let salt = C8.pack "qzyelonm"
  -- let hashfn = HashFn $ superHash salt
  -- let ans = show $ head $ findKeys hashfn :: Text
  let ans = "20864" :: Text
  putStrLn $ mappend "day14-2: " ans


run :: IO ()
run = do
  part1
  part2

