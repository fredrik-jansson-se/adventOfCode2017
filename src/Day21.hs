{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day21 where

import Protolude
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

part1 :: IO ()
part1 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day3-1: " ans

part2 :: IO ()
part2 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day3-2: " ans


run :: IO ()
run = do
  part1
  part2

