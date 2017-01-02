{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day16 where

import Protolude
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

a2b :: S.String -> S.String
a2b d = invert' d []
  where
    invert' :: S.String -> S.String -> S.String
    invert' [] acc       = acc
    invert' ('0':xs) acc = invert' xs ('1':acc)
    invert' ('1':xs) acc = invert' xs ('0':acc)

expandA :: S.String -> S.String
expandA a = a ++ ['0'] ++ a2b a

expandUntil :: Int -> S.String -> S.String
expandUntil size a 
  | length a >= size = checksum $ take size a
  | otherwise        = expandUntil size $ expandA a

checksum :: S.String -> S.String
checksum str = if even $ length theCS
                 then checksum theCS
                 else theCS
  where
    cs acc (a:b:xs) = cs (csC a b : acc) xs
    cs acc _ = reverse acc
    csC a b 
      | a == b    = '1'
      | otherwise = '0'

    theCS = cs [] str
      

part1 :: IO ()
part1 = 
  print $ expandUntil 272 "00101000101111010"

part2 :: IO ()
part2 = 
  print $ expandUntil 35651584 "00101000101111010"


run :: IO ()
run = do
  part1
  part2

