module Day5 where

import qualified Crypto.Hash.MD5 as MD5
import Data.Array
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.List (elemIndex)

hash :: String -> Int -> String -> String
hash key idx pwd | length(pwd) == 8 = reverse pwd 
                 | otherwise = case h of
                                 ('0':'0':'0':'0':'0':p:_) -> hash key nextIdx (p:pwd)
                                 _ -> hash key nextIdx pwd
  where
    c = key ++ show idx
    h = C8.unpack $ B16.encode $ MD5.hash $ C8.pack c
    nextIdx = idx + 1

hash2 :: String -> Int -> Set.Set Int -> Array Int Char -> String
hash2 key idx positions pwd 
  | Set.null positions = elems pwd
  | otherwise = case h of
                  ('0':'0':'0':'0':'0':pos:char:_) -> 
                    if Set.member (getPos pos) positions
                       then hash2 key nextIdx (Set.delete (getPos pos) positions) (newPwd pos char)
                       else hash2 key nextIdx positions pwd
                  _ -> hash2 key nextIdx positions pwd
  where
    teststr = key ++ show idx
    h = C8.unpack $ B16.encode $ MD5.hash $ C8.pack teststr
    nextIdx = idx + 1
    getPos :: Char -> Int
    getPos pos = fromJust $ elemIndex pos "0123456789abcdef"
    newPwd :: Char -> Char -> Array Int Char
    newPwd pos c = pwd // [(getPos pos, c)]

part1 :: IO ()
part1 = do
  -- let input = "reyedfim"
  -- let pwd = hash input 0 ""
  let pwd = "f97c354d"
  putStrLn $ "day5 - 1 " ++ pwd

part2 :: IO ()
part2 = do
  -- let input = "reyedfim"
  -- let pwd = hash2 input 0 (Set.fromList [0..7]) $ array (0,7) [(i,'0') | i <- [0..7]]
  let pwd = "863dde27"
  putStrLn $ "day5 - 2 " ++ pwd

run :: IO()
run = do 
  part1
  part2
