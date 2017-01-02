{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day15 where

import Protolude
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

data Disk = Disk { 
  dNum :: Int,
  dPositions :: Int,
  dPos :: Int 
  } deriving (Show)

-- Disc #1 has 13 positions; at time=0, it is at position 1.
-- Disc #2 has 19 positions; at time=0, it is at position 10.
-- Disc #3 has 3 positions; at time=0, it is at position 2.
-- Disc #4 has 7 positions; at time=0, it is at position 1.
-- Disc #5 has 5 positions; at time=0, it is at position 3.
-- Disc #6 has 17 positions; at time=0, it is at position 5.
init1 :: [Disk]
init1 = [
  Disk 1 13 1,
  Disk 2 19 10,
  Disk 3 3 2,
  Disk 4 7 1,
  Disk 5 5 3,
  Disk 6 17 5]

init2 :: [Disk]
init2 = init1 ++ [Disk 7 11 0]

advanceDisk :: Disk -> Disk
advanceDisk disk = 
  let 
    max = dPositions disk
    curPos = dPos disk
    newPos = (curPos + 1)  `mod` max
  in
  disk { dPos = newPos }

advanceN :: (Num t, Eq t) => t -> Disk -> Disk
advanceN 0 disk = disk
advanceN n disk = advanceN n' disk'
  where 
    disk' = advanceDisk disk
    n' = n - 1

advanceDiskWithPos :: Disk -> Disk
advanceDiskWithPos disk = advanceN (dNum disk) disk

allAtPos0 :: [Disk] -> Bool
allAtPos0 = all (\disk -> dPos disk == 0)

runDisks disks t 
  | allAtPos0 disks = t
  | otherwise       = runDisks disks' t'
  where
    disks' = map advanceDisk disks
    t' = t + 1

part1 :: IO ()
part1 = do
  let discountedInit = map advanceDiskWithPos init1
  let ans = runDisks discountedInit 0
  print "day15-1: " 
  print ans

part2 :: IO ()
part2 = do
  let discountedInit = map advanceDiskWithPos init2
  let ans = runDisks discountedInit 0
  print "day15-2: " 
  print ans


run :: IO ()
run = do
  part1
  part2

