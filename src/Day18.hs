{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day18 where

import Protolude
import Text.Parsec as P
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Number
import qualified Data.Map.Strict as M
import Data.List ((!!))

type Reg = Char

data Src = RegSrc Reg
         | IntSrc Int
         deriving (Show)

data Cmd = Set Reg Src
         | Mul Reg Src
         | Add Reg Src
         | Jgz Reg Src
         | Mod Reg Src
         | Snd Reg
         | Rcv Reg
         deriving (Show)

execute :: Cmd -> Context -> Context
execute (Set r src) c@Context {cLine = line, cRegs = regs} = c {cLine = line', cRegs = regs'}
  where
    line' = line+1
    regs' = setVal r ( getVal src regs) regs
execute (Mul r src) c@Context {cLine = line, cRegs = regs} = c {cLine = line', cRegs = regs'}
  where
    line' = line+1
    a = getVal (RegSrc r) regs
    b = getVal src regs
    prod = a * b
    regs' = setVal r prod regs
execute (Add r src) c@Context { cLine = line, cRegs = regs} = c {cLine = line', cRegs = regs'}
  where
    line' = line+1
    a = getVal (RegSrc r) regs
    b = getVal src regs
    sum = a + b
    regs' = setVal r sum regs
execute (Jgz r src) c@Context { cLine = line, cRegs = regs} = c {cLine = line'}
  where
    x = getVal (RegSrc r) regs
    y = getVal src regs
    linediff = if x > 0 then y else 1
    line' = line + linediff
execute (Mod r src) c@Context { cLine = line, cRegs = regs} = c {cLine = line', cRegs = regs'}
  where
    line' = line+1
    x = getVal (RegSrc r) regs
    y = getVal src regs
    reminder = x `rem` y
    regs' = setVal r reminder regs
execute (Snd r) c@Context { cLine = line, cRegs = regs}= c {cLine = line', cSounds = sounds'}
  where
    line' = line+1
    x = getVal (RegSrc r) regs
    sounds' = ((r, x):cSounds c)
execute (Rcv r) c@Context { cLine = line, cRegs = regs, cSounds = sounds} = c {cLine = line', cRecovered = recov' }
  where
    line' = line+1
    x = getVal (RegSrc r) regs
    rval = sounds !! 0
    recov = cRecovered c
    recov' = if x /= 0 then rval:recov else recov

sourceReg :: Parsec Text () Src
sourceReg = P.try $ do
  s <- letter
  P.optional newline
  return $ RegSrc s

sourceInt :: Parsec Text () Src
sourceInt = P.try $ do
  s <- int
  return $ IntSrc s

source :: Parsec Text () Src
source = P.try $ sourceReg P.<|> sourceInt

binaryCmd :: Parsec Text () Cmd
binaryCmd = do
  cmd <- ts "set" P.<|> ts "mul" P.<|> ts "add" P.<|> ts "jgz" P.<|> ts "mod"
  spaces
  dest <- anyChar
  spaces
  src <- source
  (P.optional newline) P.<|> eof
  return $ case cmd of
    "set" -> Set dest src
    "mul" -> Mul dest src
    "add" -> Add dest src
    "jgz" -> Jgz dest src
    "mod" -> Mod dest src
  where
    ts :: [Char] -> Parsec Text () [Char]
    ts s = P.try $ string s

unaryCmd :: Parsec Text () Cmd
unaryCmd = do
  cmd <- ts "snd" P.<|> ts "rcv"
  spaces
  dest <- anyChar
  (P.optional newline) P.<|> eof
  return $ case cmd of
    "snd" -> Snd dest
    "rcv" -> Rcv dest
  where
    ts :: [Char] -> Parsec Text () [Char]
    ts s = P.try $ string s

command :: Parsec Text () Cmd
command = (P.try binaryCmd) P.<|> (P.try unaryCmd)

commands :: Text -> [Cmd]
commands txt = case P.parse (many1 command) "Day18" txt of
  Right cmds -> cmds
  Left err -> traceShow err []
  
type R = M.Map Reg Int
type Sounds = [(Reg, Int)]

getVal :: Src -> R -> Int
getVal (IntSrc v) _ = v
getVal (RegSrc v) r = M.findWithDefault 0 v r

setVal :: Reg -> Int -> R -> R
setVal r v regs = M.insert r v regs

data Context = Context {
  cLine :: Int,
  cProgram :: [Cmd],
  cRegs :: R,
  cSounds :: Sounds,
  cRecovered :: Sounds
} deriving (Show)


run :: Context -> Context 
run c@Context{cLine = line, cProgram=cmds} = let
  cmd = cmds !! line
  c' = execute cmd c
  line' = cLine c'
  rcv = cRecovered c'
  in
    if line' < 0 || line' >= length cmds || length rcv > 0
      then c'
      else run c'

solve1 :: Text -> Int
solve1 txt = let
  cmds = commands txt
  ctx = run $ Context {cLine = 0, cProgram = cmds, cRegs = M.empty, cSounds = [], cRecovered = [] }
  recv = cRecovered ctx
  rval = snd $ recv !! 0
  in
    rval
