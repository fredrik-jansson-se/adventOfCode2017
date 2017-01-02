module Two where

data Position = Pos Int Int
  deriving(Show)

showPos :: Position -> String
showPos (Pos 2 0) = "1"

showPos (Pos 1 1) = "2"
showPos (Pos 2 1) = "3"
showPos (Pos 3 1) = "4"

showPos (Pos 0 2) = "5"
showPos (Pos 1 2) = "6"
showPos (Pos 2 2) = "7"
showPos (Pos 3 2) = "8"
showPos (Pos 4 2) = "9"

showPos (Pos 1 3) = "A"
showPos (Pos 2 3) = "B"
showPos (Pos 3 3) = "C"

showPos (Pos 2 4) = "D"

{- 2nd
     1
  2 3 4
5 6 7 8 9
  A B C
    D
-}

isValid :: Position -> Bool
isValid (Pos x y) =
  any (==y) [0..4] && any (==x) row
  where
    valids = [
              [2..2],
              [1..3],
              [0..4],
              [1..3],
              [2..2]
             ]
    row = valids !! y


data Move = U
          | D
          | L
          | R
          deriving(Show, Eq)

char2move :: Char -> Move
char2move c = case c of
                'U' -> Two.U
                'D' -> Two.D
                'L' -> Two.L
                'R' -> Two.R

parseLine :: String -> [Move]
parseLine = map char2move

validPos :: Int -> Int
validPos a | a < 0 = 0
           | a > 2 = 2
           | otherwise = a


move :: Position -> Move -> Position
-- move (Pos x y) m | m == U = Pos x $ validPos $ y - 1
--                  | m == D = Pos x $ validPos $ y + 1
--                  | m == L = Pos (validPos $ x - 1) y
--                  | m == R = Pos (validPos $ x + 1) y
move oldPos@(Pos x y) m = 
  let newPos = case m of
                Two.U -> Pos x (y-1)
                Two.D -> Pos x (y+1)
                Two.L -> Pos (x-1) y
                Two.R -> Pos (x+1) y
  in
    if isValid newPos 
       then newPos
       else oldPos

applyMoves :: (Position, String) -> [Move] -> (Position, String)
applyMoves (pos, codes) mvs = 
  let endPos = foldl move pos mvs
  in
    (endPos, showPos endPos ++ codes)

run :: IO String
run = do 
  d <- readFile "day2/input"
  -- let d = "ULL\nRRDDD\nLURDL\nUUUUD"
  let lns = map parseLine $ lines d 
  let startPos = Pos 1 1 
  let last = foldl applyMoves (startPos, "") lns
  let code = reverse $ snd last
  return $ "two = " ++ code


{-
You arrive at Easter Bunny Headquarters under cover of darkness. However, you
left in such a rush that you forgot to use the bathroom! Fancy office buildings
like this one usually have keypad locks on their bathrooms, so you search the
front desk for the code.

"In order to improve security," the document you find says, "bathroom codes
will no longer be written down. Instead, please memorize and follow the
procedure below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by
starting on the previous button and moving to adjacent buttons on the keypad: U
moves up, D moves down, L moves left, and R moves right. Each line of
instructions corresponds to one button, starting at the previous button (or,
for the first line, the "5" button); press whatever button you're on at the end
of each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk
to the bathroom. You picture a keypad like this:

1 2 3
4 5 6
7 8 9
Suppose your instructions are:

ULL
RRDDD
LURDL
UUUUD
- You start at "5" and move up (to "2"), left (to "1"), and left (you can't,
  and stay on "1"), so the first button is 1.
- Starting from the previous button ("1"), you move right twice (to "3") and
  then down three times (stopping at "9" after two moves and ignoring the third),
  ending up with 9.
- Continuing from "9", you move left, up, right, down, and left, ending with 8.
- Finally, you move up four times (stopping at "2"), then down once, ending with 5.
So, in this example, the bathroom code is 1985.

Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?
-}
