{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day16 where

import Protolude
import qualified Data.Text as T
import qualified Data.String as S
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as M
import Control.Monad.Primitive

type Pos = Int
type Program = Char
data Cmd = Spin Int
         | Exchange Pos Pos
         | Partner Program Program
         deriving (Show)

parseSpin :: Parsec Text () Cmd
parseSpin = Text.Parsec.Prim.try $ do
  char 's'
  num <- int
  return $ Spin num

parseExchange :: Parsec Text() Cmd
parseExchange = Text.Parsec.Prim.try $ do
  char 'x'
  from <- int
  char '/'
  to <- int
  return $ Exchange from to

parsePartner :: Parsec Text() Cmd
parsePartner = Text.Parsec.Prim.try $ do
  char 'p'
  from <- anyChar
  char '/'
  to <- anyChar
  return $ Partner from to

parseCmd :: Parsec Text () Cmd
parseCmd = parseSpin P.<|> parseExchange P.<|> parsePartner

program :: Parsec Text () [Cmd]
program = parseCmd `sepBy` (char ',')

-- type VT = IO (MV.MVector RealWorld Char)
type VT = V.Vector Char

type MC = M.IntMap Char

execute :: Text -> Cmd -> Text
execute txt (Spin pos) = let
  l = T.length txt
  (a,b) = T.splitAt (l - pos) txt
  in T.append b a
execute txt (Exchange pos1 pos2) = let
  r1 = T.index txt pos1
  r2 = T.index txt pos2
  txt' = T.map (\c -> if c == r1 then r2 else if c == r2 then 'x' else c) txt
  txt'' = T.map (\c -> if c == 'x' then r1 else c) txt'
  in
    execute txt (Partner r1 r2)
execute txt (Partner r1 r2) = let
  txt' = T.map (\c -> if c == r1 then r2 else if c == r2 then 'x' else c) txt
  txt'' = T.map (\c -> if c == 'x' then r1 else c) txt'
  in
    txt''

executeVT :: VT -> Cmd -> VT
executeVT v (Spin pos) = let
  s = V.length v
  (a,b) = V.splitAt (s-pos) v
  in b V.++ a
executeVT v (Exchange p1 p2) = let
  c1 = v V.! p1
  c2 = v V.! p2
  in
    V.imap (\idx v -> if idx == p1 then c2 else if idx == p2 then c1 else v) v
executeVT v (Partner c1 c2) = let
  p1 = case V.elemIndex c1 v of Just p -> p
  p2 = case V.elemIndex c2 v of Just p -> p
  in
    executeVT v (Exchange p1 p2)

executeM :: MC -> Cmd -> MC
executeM v (Spin pos) = 
  let s = M.size v
  in
    M.mapKeys (\k -> (k + pos) `rem` s) v
executeM v (Exchange p1 p2) = let
  c1 = v M.! p1
  c2 = v M.! p2
  in
    M.insert p1 c2 $ M.insert p2 c1 v
executeM v (Partner c1 c2) = 
  M.map (\c -> if c == c1 then c2 else if c == c2 then c1 else c) v

toMap:: Text -> MC
toMap txt = foldl' (\m (i,c) -> M.insert i c m) M.empty $ zip [0..] (T.unpack txt)

fromMap :: MC -> Text
fromMap m = T.pack $ map snd $ M.toAscList m

runProgram :: [Cmd] -> Text -> Text
runProgram cmds regs = foldl' execute regs cmds

runProgramVT :: [Cmd] -> VT -> VT
runProgramVT cmds regs = foldl' executeVT regs cmds

runProgramM :: [Cmd] -> MC -> MC
runProgramM cmds regs = foldl' executeM regs cmds

toVector :: Text -> VT
toVector txt = let
  v = V.fromList $ T.unpack txt
  in 
    v

fromVector :: VT -> Text
fromVector v = let
  s = V.toList v
  in T.pack s

solve1 :: Text -> Text -> IO Text
solve1 input regs = let
  (h:_) = T.lines input
  cmds = case P.parse program "day16" input of
            Right r -> r
  in 
    return $ fromMap $ runProgramM cmds $ toMap regs

solve2 :: Text -> Text -> Int -> Text
solve2 input regs rounds = let
  (h:_) = T.lines input
  vregs = toMap regs
  cmds = case P.parse program "day16" input of
          Right r -> r
  r = foldl' (\txt idx -> if idx `rem` 10 == 0 then traceShow idx $ runProgramM cmds txt else runProgramM cmds txt) vregs [1..rounds]
  in 
    fromMap r
