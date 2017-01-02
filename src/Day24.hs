{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day24 where

import Protolude
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion (fromByteString)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

part1 :: IO ()
part1 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day24-1: " ans

part2 :: IO ()
part2 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day24-2: " ans


run :: IO ()
run = do
  part1
  part2

