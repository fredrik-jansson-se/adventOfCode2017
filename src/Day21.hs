{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day21 where

import Protolude

import qualified Base as PBase
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion (fromByteString)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE

-- type Pwd = V.Vector Char
type Pwd = ByteString
data Cmd = Cmd { runCmd :: Pwd -> Pwd } 

move :: Int -> Int -> Pwd -> Pwd
move from to pwd = mappend f' $ BS.cons c b'
  where
    (f, b) = BS.splitAt from pwd
    s' = mappend f $ BS.tail b
    c = BS.head b
    (f', b') = BS.splitAt to s'

parseMove :: ByteString -> Maybe Cmd
parseMove str = do
  let regex = "move position (\\d+) to position (\\d+)" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  [_,f,t] <- case res of 
              [] -> Nothing
              _  -> Just res
  from <- fromByteString f
  to <- fromByteString t
  return $ Cmd $ move from to

fixSteps str steps 
  | steps < 0 = fixSteps str (steps + BS.length str)
  | steps >= BS.length str = fixSteps str (steps - BS.length str)
  | otherwise = steps

rotate :: Int -> Pwd -> Pwd
rotate steps pwd = mappend b a
  where
    idx = fixSteps pwd steps
    (a,b) = BS.splitAt idx pwd
  

parseRotate :: ByteString -> Maybe Cmd
parseRotate str = do
  let regex = "rotate (left|right) (\\d+) steps?" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  [_,dir,s] <- case res of 
                [] -> Nothing
                _  -> Just res
  abs_steps <- fromByteString s
  let steps = if BS.isPrefixOf "left" dir
                then abs_steps
                else -abs_steps
  return $ Cmd $ Day21.rotate steps

swapPositions :: Int -> Int -> ByteString -> ByteString
swapPositions p1 p2 str = str''
  where
    (f1, b1) = BS.splitAt p1 str
    c2 = BS.index str p2
    str' = mappend f1 (BS.cons c2 (BS.tail b1))
    (f2, b2) = BS.splitAt p2 str'
    c1 = BS.index str p1
    str'' = mappend f2 (BS.cons c1 (BS.tail b2))

swapLetters :: Word8 -> Word8 -> ByteString -> ByteString
swapLetters c1 c2 str = swapPositions c1idx c2idx str
  where
    Just c1idx = BS.elemIndex c1 str
    Just c2idx = BS.elemIndex c2 str

parseSwapLetter :: ByteString -> Maybe Cmd
parseSwapLetter str = do
  let regex = "swap letter (\\w) with letter (\\w)" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  [_,c1,c2] <- case res of 
                [] -> Nothing
                _  -> Just res
  return $ Cmd $ Day21.swapLetters (BS.head c1) (BS.head c2)

parseSwapPosition :: ByteString -> Maybe Cmd
parseSwapPosition str = do
  let regex = "swap position (\\d) with position (\\d)" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  [_,c1,c2] <- case res of 
                [] -> Nothing
                _  -> Just res
  p1 <- fromByteString c1
  p2 <- fromByteString c2
  return $ Cmd $ Day21.swapPositions p1 p2

rotateOnLetter :: Word8 -> ByteString -> ByteString
rotateOnLetter c str = Day21.rotate (-rot) str
  where
    Just cIdx = BS.elemIndex c str
    rot = 1 + cIdx + if cIdx >= 4 then 1 else 0

parseRotateLetter :: ByteString -> Maybe Cmd
parseRotateLetter str = do
  let regex = "rotate based on position of letter (\\w)" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  [_,c1] <- case res of 
              [] -> Nothing
              _  -> Just res
  return $ Cmd $ rotateOnLetter (BS.head c1)

reverse start end str = str'
  where
    (a,r) = BS.splitAt start str
    (toRev, b) = BS.splitAt (1 + end - start) r
    rev = BS.reverse toRev
    str' = mappend a $ mappend rev b

parseReverse :: ByteString -> Maybe Cmd
parseReverse str = do
  let regex = "reverse positions (\\d+) through (\\d)" :: ByteString
  let matches = str =~ regex :: AllTextSubmatches [] ByteString
  let res = getAllTextSubmatches matches :: [ByteString]
  [_,s,e] <- case res of 
              [] -> Nothing
              _  -> Just res
  start <- fromByteString s
  end <- fromByteString e
  return $ Cmd $ Day21.reverse start end

parse :: ByteString -> Cmd
parse str = L.head $ mapMaybe (\fn -> fn str) [parseMove, parseRotate, parseSwapLetter, parseSwapPosition, parseRotateLetter, parseReverse]

str2vec :: ByteString -> Pwd
str2vec a = a

part1 :: IO ()
part1 = do
  -- print $ Day21.rotate 6 "ecabd"
  -- let cmd = parse "rotate based on position of letter d"
  -- let res = runCmd cmd "ecabd"
  -- print res
  let pwd = str2vec "abcdefgh"
  tcmd <- BS.readFile "day21" 
  -- let pwd = str2vec "abcde"
  -- let tcmd = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d"
  let cmds = map parse $ C8.lines tcmd :: [Cmd]
  let res = foldl (flip runCmd) pwd cmds
  print res
  let ans = "ok" :: Text
  putStrLn $ mappend "day21-1: " ans
  -- defchgab

part2 :: IO ()
part2 = do
  let ans = "ok" :: Text
  putStrLn $ mappend "day21-2: " ans
  -- fbhcgaed

run :: IO ()
run = do
  part1
  part2

