{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day11 where

import Protolude
import Data.Array
-- import qualified Base as PBase
import qualified Data.Text.Lazy as T

import Data.List(intersperse)

data Floor = Floor {floor::Int, e::Bool, hg::Bool, hm::Bool, lg::Bool, lm::Bool}

showFloor :: Floor -> LText
showFloor (Floor num e hg hm lg lm) = T.intercalate space ss
     where
       space = " " :: LText
       ss :: [LText]
       ss = [mappend "F" $ show num,
             if e then "E" else ".",
             if hg then "HG" else ". ",
             if hm then "HM" else ". ",
             if lg then "LG" else ". ",
             if lm then "LM" else ". "]

validFloor :: Floor -> Bool
-- Can't have HM and LG only
validFloor f 
-- Can't have HM and LG only
  | hm f && lg f && (not $ hg f) = False
-- Can't have HG and LM only
  | lm f && hg f && (not $ lg f) = False
validFloor (Floor _ _ True  False _    True) = False
validFloor _ = True

type GameState = Array Int Floor
validState :: GameState -> Bool
validState = all validFloor

{-
 F4 .  .  .  .  .  
 F3 .  .  .  LG .  
 F2 .  HG .  .  .  
 F1 E  .  HM .  LM 
-}

data Move = Move { doMove :: Floor -> (Floor, Floor) }

moveHG :: Floor -> (Floor, Floor)
moveHG = undefined


initState1 :: GameState
initState1 = listArray (1,4) [ Floor 1 True False True False True,
                              Floor 2 False True False False False,
                              Floor 3 False False False True False,
                              Floor 4 False False False False False
                            ]
showState :: GameState -> LText
showState gs = T.unlines $ [showFloor $ gs ! f | f <- [4,3,2,1]]

part1 :: IO ()
part1 = do
  let ans = "ok" :: Text
  print $ showFloor (Floor 1 True False True False True)
  let is = initState1
  putStrLn $ showState is
  let valid = map validFloor is
  putStrLn $ (show valid :: Text)

  print $ "day11-1: " ++ show ans

part2 :: IO ()
part2 = do
  let ans = "ok"
  putStrLn $ "day11-2: " ++ ans


run :: IO ()
run = do
  part1
  part2

