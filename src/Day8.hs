{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day8 where

import Protolude
import Common
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number

-- cpv inc 669 if csu >= -6
register :: Parsec T.Text () T.Text
register = do 
  reg <- many1 letter
  spaces
  return $ T.pack reg

data Cmd = Cmd (Int -> Int -> Int) Text Int

cmd :: Parsec T.Text () Cmd
cmd = do
  reg <- register
  c <- (string "inc") P.<|> (string "dec")
  spaces
  val <- int
  spaces
  return $ case c of 
          "inc" -> Cmd (+) reg val
          "dec" -> Cmd (-) reg val

data Cmp = Cmp (Int -> Int -> Bool) Text Int 

op :: Parsec T.Text () (Int -> Int -> Bool)
op = do 
  -- op <- string "<=" P.<|> le P.<|> string ">=" P.<|> string ">" P.<|> string "==" P.<|> string "!="
  op <- manyTill anyChar (P.try (char ' '))
  spaces
  return $ case op of
            "<" -> (<)
            "<=" -> (<=)
            ">" -> (>)
            ">=" -> (>=)
            "==" -> (==)
            "!=" -> (/=)

cmp :: Parsec T.Text () Cmp
cmp = do
  string "if"
  spaces
  reg <- liftM T.pack $ many1 letter
  spaces
  o <- op
  val <- int
  return $ Cmp o reg val 

line :: Parsec T.Text () Day8.Line
line = do
  c <- cmd
  cp <- cmp
  return (c, cp)

type Line = (Cmd, Cmp)

type MM = M.Map Text Int

getReg :: Text -> MM -> Int
getReg reg = M.findWithDefault 0 reg

updateReg :: Text -> Int -> MM -> MM
updateReg reg val m = let
  oldMax = getReg "the-max" m
  m' = if val > oldMax then M.insert "the-max" val m else m
  in M.insert reg val m'

eval :: Cmp -> MM -> Bool
eval (Cmp op reg val) m = op rv val
  where
    rv = getReg reg m

runCmd :: Cmd -> MM -> MM
runCmd (Cmd op reg val) m = let
  rv = getReg reg m
  nval = op rv val
  m' = updateReg reg nval m
  in m'

run1 :: [Day8.Line] -> MM -> MM
run1 [] m = m
run1 (x:xs) m = let
  cmd = fst x
  cmp = snd x
  r = eval cmp m 
  m' = runCmd cmd m
  in if r then run1 xs m'
       else run1 xs m
   
fromRight (Right a) = a

solve1 :: Text -> Int
solve1 txt = let 
  ll = T.lines txt
  p = map (\l -> (l, P.parse line "day8" l)) ll
  pp = map (fromRight. snd) p
  r = filter (\(k, _) -> k /= "the-max") $ M.toList $ run1 pp M.empty
  in maximum $ map snd r 

solve2 :: Text -> Int
solve2 txt = let 
  ll = T.lines txt
  p = map (\l -> (l, P.parse line "day8" l)) ll
  pp = map (fromRight. snd) p
  r = run1 pp M.empty
  in 
  getReg "the-max" r
