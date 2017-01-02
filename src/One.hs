module One where

import Text.Read
import Data.Char (isSpace, isDigit)
import Data.List (find)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace

data Move = Right Int
          | Left Int

data Direction = North
               | South
               | West
               | East
               deriving(Show, Eq)

data Position = Pos Direction Int Int
  deriving(Show)

instance Eq Position where
  (Pos _ x1 y1) == (Pos _ x2 y2) = x1==x2 && y1==y2

instance Ord Position where
  (Pos _ x1 y1) <= (Pos _ x2 y2) = x1 <= x2 || y1 <= y2

instance Show Move where
  show (One.Right i) = "R"++show i
  show (One.Left i) = "L"++show i

instance Read Move where
  readsPrec _ input = 
    let cleanI = dropWhile isSpace input
        t = head cleanI
        (lenS, rest) = span isDigit $ tail cleanI
        len = read lenS :: Int
        in
          case t of
            'R' -> [(One.Right len, rest)]
            'L' -> [(One.Left len, rest)]
            otherwise -> []

startPos :: Position
startPos = Pos North 0 0

newDirection :: Direction -> Move -> Direction
newDirection North (One.Left _) = West
newDirection West (One.Left _) = South
newDirection South (One.Left _) = East
newDirection East (One.Left _) = North
newDirection North (One.Right _) = East
newDirection West (One.Right _) = North
newDirection South (One.Right _) = West
newDirection East (One.Right _) = South

length :: Move -> Int
length (One.Left i) = i
length (One.Right i) = i

updatePos :: [Position] -> Move -> [Position]
updatePos ps m = 
  let (Pos dir x y) = head ps
      newDir = newDirection dir m
      len = One.length m
      update North = (0, 1)
      update East = (1, 0)
      update South = (0, -1)
      update West  = (-1, 0)
      (dx,dy) = update newDir
      createNewPos i = Pos newDir (x + i * dx) (y + i * dy)
      newPositions = reverse $ map createNewPos [1..len]
  in
    newPositions ++ ps

updateCnt :: Position -> Map.Map Position Int -> Map.Map Position Int
updateCnt p m =
  let corr = (Pos West (-176) (-5))
      newCnt = 1 + Map.findWithDefault 0 p m
  in
    if p == corr 
       then Map.insert p newCnt m
       else Map.insert p newCnt m

okCnt m p =
  let cnt = case Map.lookup p m of
             (Just c) -> c
             Nothing -> 0
  in
    cnt > 1


run :: IO String
run = do 
  movementData <- readFile "day1/input"
  -- let movementData ="R8, R4, R4, R8"
  -- let movementData = "L1, L3, L5, L3, R1, L4, L5"
  let movementsS = splitOn "," movementData
  let movements = map read movementsS :: [Move]
  let positions = reverse $ foldl updatePos [startPos] movements
  let posLen = Prelude.length positions
  putStrLn $ "Number of positions = " ++ show posLen
  let posSet = foldr updateCnt Map.empty positions
  let correct = (Pos West (-176) (-5))
  -- putStrLn $ show posSet
  -- putStrLn $ show positions
  let firstDup = find (okCnt posSet) positions
  putStrLn $ "first = " ++ show firstDup
  return "1/1 = "

a = Pos West (-176) (-5)
b = Pos North (-176) (-5)

-- Pos North (-161) (-130)
-- Pos South (-3) 1
-- Correct: Pos West (-176) (-5)
