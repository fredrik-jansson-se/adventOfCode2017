{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day19 where

import Protolude
import qualified Data.IntMap.Strict as M
import Data.List ((!!), span, tail)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

createInit :: Int -> M.IntMap Int
createInit size = M.fromList [(i, 1) | i <- [1..size]]

type NextIdx = Int -> M.IntMap Int -> Int

getLeftIndex :: Int -> M.IntMap Int -> Int
getLeftIndex idx m =
  case M.lookupGT idx m of
    Just (k, _) -> k
    Nothing -> fst . fromJust $ M.lookupGT 0 m

getNextLeftIdx :: Int -> M.IntMap Int -> Int
getNextLeftIdx start m = 
  case M.lookup next m of
    Just 0 -> getNextLeftIdx next m
    Just _ -> trace log next
    Nothing -> getNextLeftIdx next m
  where
    next = getLeftIndex start m
    log = show start ++ " next is " ++ show next

getOppositeIdx :: Int -> M.IntMap Int -> Int
getOppositeIdx start m =
  case M.lookup next m of
    Just _ -> trace log next
  where
    keys = M.keys m
    (lk, hk) = break (==start) keys
    -- wrappedKeys = hk ++ lk
    half_size = M.size m `div` 2
    next = if half_size < length hk
              then hk !! half_size
              else lk !! (half_size - length hk)
    -- next = wrappedKeys !! half_size
    log = show start ++ " -> " ++ show next ++ " hs: " ++ show half_size 


winner :: M.IntMap Int -> Maybe (Int, Int)
winner m = if M.size m > 1
              then Nothing
              else case M.toList moreThanOne of
                     (a:[]) -> Just a
                     _ -> Nothing
  where
    moreThanOne = M.filter (>0) m

update :: NextIdx -> M.IntMap Int -> M.IntMap Int
update nifn m = foldl' f m $ M.keys m
  where
    f :: M.IntMap Int -> Int -> M.IntMap Int
    f m k = 
      case M.lookup k m of
        Just 0 -> m
        Just cnt -> 
          case M.lookup next m of
                  -- 0 to the left of us
                  Just 0 -> m
                  Just n -> M.delete next $ M.insert k (cnt+n) m
                  -- Just n -> M.insert next 0 $ M.insert k (cnt+n) m
        _ -> m
     where
       next = nifn k m

runPart1 :: M.IntMap Int -> (Int, Int)
runPart1 m = case winner m of
               Just a -> a
               Nothing -> runPart1 $ update getNextLeftIdx m


runPart2 :: M.IntMap Int -> (Int, Int)
runPart2 m = 
  case winner m of
    Just a -> a
    Nothing -> runPart2 $ update getOppositeIdx m

part1 :: IO ()
part1 = do
  -- let init = createInit 3014387
  -- let s = runPart1 init
  -- let ans = show s :: Text
  let ans = "1834471" :: Text
  putStrLn $ mappend "day19-1: " ans

part2 :: IO ()
part2 = do
  -- let init = createInit 5
  -- let init = createInit 3014387
  -- let s = runPart2 init
  -- let ans = show s :: Text
  let ans = "ok" :: Text
  putStrLn $ mappend "day19-2: " ans


run :: IO ()
run = do
  part1
  part2

