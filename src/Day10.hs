-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Day10 where

-- import Protolude
-- import qualified Base as PBase
-- import Data.List (splitAt)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.String as S
-- import qualified Data.Text as T
-- import qualified Data.Text.Read as TR
-- import qualified Data.Text.IO as TIO
import Debug.Trace
import Text.Regex.PCRE
-- import Text.Show

type ID = Int
data Target = Bot ID [Int]
            | Output ID (Maybe Int)
            deriving (Show)

data Type = TBot | TOutput deriving (Eq, Show)

type Bots = M.Map ID Target
type Outputs = M.Map ID Target

type Lookup = (Outputs, Bots)

data Give = Give ID (Type, ID) (Type, ID)
          | Value (ID, Int)
          deriving (Show)

toInt :: String -> Int
toInt t = case reads t of
            [(i,_)] -> i


parseGive :: String -> Maybe (ID, (Type, ID), (Type, ID))
parseGive str = 
  case m of
    [] -> Nothing
    _  -> Just targets
  where
    r = "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)" :: String
    (_, _, _, m) = str =~ r :: (String, String, String, [String])
    (a:typeLow:b:typeHigh:c:_) = m
    sourceID = toInt a
    low = case typeLow of
            "bot"    -> (TBot, toInt b)
            "output" -> (TOutput, toInt b)
    high = case typeHigh of
             "bot"    -> (TBot, toInt c)
             "output" -> (TOutput, toInt c)
    targets = (sourceID, low, high)

-- value 41 goes to bot 204
parseValue :: String -> Maybe (ID, Int)
parseValue str = 
  case m of
    [] -> Nothing
    _  -> Just values
  where
    r = "value (\\d+) goes to bot (\\d+)"
    (_, _, _, m) = str =~ r :: (String, String, String, [String])
    values = (toInt $ m !! 1, toInt $ head m )

parse :: String -> Give
parse str = case parseGive str of
              (Just (from,  toLow, toHigh)) -> Give from toLow toHigh
              _                             -> Value $ fromJust $ parseValue str


getTarget :: (Type, ID) -> Lookup -> Target
getTarget (TBot,id) (_,m) = M.findWithDefault (Bot id []) id m
getTarget (TOutput, id) (o, _) = M.findWithDefault (Output id Nothing) id o

getLowHigh :: Target -> (Int, Int)
getLowHigh (Bot id [a,b]) = trace log (min', max')
  where 
    min' = min a b
    max' = max a b
    log = if min' == 17 && max' == 61 
             then "10-1: " ++ show id
             else ""

give :: Target -> Int -> Lookup -> Lookup
give (Bot id items) val (o,b) = (o, M.insert id newBot b)
  where
    newBot = Bot id $ val:items
give (Output id _) val (o,b) = (M.insert id newOutput o, b)
  where
    newOutput = Output id $ Just val

deleteBot :: ID -> Lookup -> Lookup
deleteBot id (o, b) = (o, M.delete id b)

canGive :: Target -> Bool
canGive (Bot _ items) = length items == 2
canGive _ = False

handleGive :: Give -> Lookup -> (Bool, Lookup)
handleGive (Give fromID toLow toHigh) lu = trace log (cg, newLu)
  where
    from = getTarget (TBot, fromID) lu
    cg = canGive from
    newLu = if cg
               then luHigh
               else lu
    log = show fromID ++ " " ++ show toLow ++ " " ++ show toHigh ++ " = " ++ show cg
    (lowVal, highVal) = getLowHigh from :: (Int, Int)
    lowTarget = getTarget toLow lu
    highTarget = getTarget toHigh lu
    lu1 = deleteBot fromID lu
    luLow = give lowTarget lowVal lu1
    luHigh = give highTarget highVal luLow

handleGive (Value (toID, value)) lu = (True, newLU)
  where 
    bot = getTarget (TBot, toID) lu
    newLU = give bot value lu

handle :: [Give] -> Lookup
handle gives = handle' gives (M.empty, M.empty)
  where
    handle' [] lu = lu
    handle' (x:xs) lu = case handleGive x lu of
                          (True, newLu) -> handle' xs newLu
                          (False, _) -> handle' (xs ++ [x]) lu
                    

part1 :: IO ()
part1 = do
  -- let test = ["bot 200 gives low to bot 168 and high to bot 1",
  --             "bot 39 gives low to output 5 and high to bot 32",
  --             "bot 176 gives low to output 18 and high to output 3"]
  let input = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
  input <- readFile "day10"
  let ids = map parse $ lines input
  let res = handle ids
  -- let ans = show res
  -- putStrLn $ mappend ("day10-1: ") ans
  -- output 0 : 53
  -- output 1 : 41
  -- output 2 : 11
  putStrLn $ mappend "day10-2: " (show (53*41*11))


run :: IO ()
run = do
  part1
  return ()

