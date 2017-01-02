{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day22 where

import Protolude
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion (fromByteString)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

data Disk = Disk 
  { 
  totalDisk :: Int, 
  usedDisk :: Int, 
  freeDisk :: Int 
  } deriving (Show)

parse :: ByteString -> Maybe (Int, Int, Disk)
parse str = do 
  let regex = "/dev/grid/node\\-x(\\d+)\\-y(\\d+)\\s*(\\d+)T\\s*(\\d+)T\\s*(\\d+)T\\s*(\\d+)%" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  (_:xs:ys:total_s:used_s:avail_s:_) <- case res of
                                          [] -> Nothing
                                          _ -> Just res
  x <- fromByteString xs
  y <- fromByteString ys
  total <- fromByteString total_s
  used <- fromByteString used_s
  return (x, y, Disk total used (total - used))

type Pos = (Int, Int)
type DM = M.Map Pos Disk

add :: DM -> Maybe (Int, Int, Disk) -> DM
add m (Just (x,y,d)) = M.insert (x,y) d m 

neighbors :: Pos -> DM -> [(Int, Int)]
neighbors (x,y) m = filter (\x -> M.member x m) nbrs
  where
    nbrs = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

validPair :: Disk -> Disk -> Bool
validPair (Disk _ used _) (Disk _ _ avail) = used <= avail

validNbrs :: DM -> Pos -> [Pos]
validNbrs m p =
  let
    nbrs = neighbors p m
    this :: Disk
    this = m M.! p 
    vp n = validPair this $ m M.! n
  in
    filter vp nbrs

viablePairs :: DM -> [Pos] -> S.Set Pos
viablePairs m r = vp r S.empty
  where
    vp [] acc = acc
    vp (a:rs) acc = vp rs newAcc
      where
        a_disk = m M.! a
        mm = M.delete a m
        keys = M.keys mm
        valids = filter (\other -> validPair a_disk $ mm M.! other) keys
        log = mappend "valids len " (show $ length valids :: Text)
        newAcc = S.union acc $ S.fromList valids

part1 :: IO ()
part1 = do
  d <- C8.readFile "day22"
  let p = filter isJust $ map parse $ C8.lines d
  let lu = foldl' add M.empty p
  let keys = M.keys lu
  let valids = viablePairs lu keys
  let ans = show $ S.size valids :: Text
  putStrLn $ mappend "day22-1: " ans
  -- 906

part2 :: IO ()
part2 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day3-2: " ans


run :: IO ()
run = do
  part1
  part2

