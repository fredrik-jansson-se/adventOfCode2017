{-# LANGUAGE NoImplicitPrelude #-}

module Day9 where

import Protolude
import qualified Base as PBase
import Data.List (splitAt)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO
-- import Text.Regex.TDFA

fromRight :: Either a b -> b
fromRight (Right x) = x

getSize :: Text -> (Int, Int, Text)
getSize str = (nChars, repeat, rest)
  where
    -- skip ( using tail
    (nChars, rest1) = fromRight $ TR.decimal $ T.tail str
    -- skip x using tail
    (repeat, rest2) = fromRight $ TR.decimal $ T.tail rest1
    -- skip ) using tail
    rest = T.tail rest2

unpack :: Int -> Int -> Text -> (Text, Text)
unpack nChars nRep s = (uncompressed, rest)
  where
    (rep, rest) = T.splitAt nChars s
    uncompressed = T.replicate nRep rep

decompress :: Text -> Text
decompress = decomp (T.pack "")
  where
    decomp :: Text -> Text -> Text
    decomp acc s | T.null s = acc
                 | '(' == T.head s = decomp nacc rest
                 | otherwise = decomp (T.append acc nonSpecial) rest2
            where
              (nChars, nRep, rest1) = getSize s
              (unpacked, rest) = unpack nChars nRep rest1
              nacc = T.append acc $ unpacked
              (nonSpecial, rest2) = T.span (/='(') s

decompSize :: Text -> Integer
decompSize = decomp
  where
    decomp :: Text -> Integer
    decomp s 
      | T.null s = 0
      | '(' == T.head s = subLen + decomp newRest
      | otherwise = nonSpecialLen + decomp rest2
     where
       (nChars, nRep, rest) = getSize s
       newRest = T.drop nChars rest
       repI = toInteger nRep
       decompRest = decompSize $ T.take nChars rest
       subLen = repI * decompRest

       (nonSpecial, rest2) = T.span (/='(') s
       nonSpecialLen = toInteger $ T.length nonSpecial

part1 :: IO ()
part1 = do
  input <- readFile "day9"
  let decompressed = T.filter (/=' ') $ decompress input
  -- lenght w/o trailing newline
  let length = T.length decompressed - 1
  let ans = T.pack $ show $ length
  TIO.putStrLn $ T.append (T.pack "day9-1: ") ans

part2 :: IO ()
part2 = do
  input <- readFile "day9"
  -- lenght w/o trailing newline
  let length = decompSize input - 1
  let ans = T.pack $ show length
  TIO.putStrLn $ T.append (T.pack "day9-2: ") ans


run :: IO ()
run = do
  part1
  part2

