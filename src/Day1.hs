{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day1 where

import Protolude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion (fromByteString)

part1 :: IO ()
part1 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day1-1: " ans

part2 :: IO ()
part2 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day1-2: " ans


run :: IO ()
run = do
  part1
  part2

