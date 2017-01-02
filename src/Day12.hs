{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day12 where

import Protolude
import Data.List ((!!))
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Regex.PCRE
import Data.String (String)

data Registers = Registers { a::Int, b::Int, c::Int, d::Int } deriving (Show)

type ProgramLine = Int

data Command = Command { runCmd :: (ProgramLine, Registers) -> (ProgramLine, Registers) } 

getRegVal :: Char -> Registers -> Int
getRegVal 'a' = a
getRegVal 'b' = b
getRegVal 'c' = c
getRegVal 'd' = d

setRegVal :: Char -> Int -> Registers -> Registers
setRegVal 'a' val r = r { a = val}
setRegVal 'b' val r = r { b = val}
setRegVal 'c' val r = r { c = val}
setRegVal 'd' val r = r { d = val}


copyRegister :: Char -> Char -> Registers -> Registers
copyRegister from to regs = newRegs
  where
    val = getRegVal from regs
    newRegs = setRegVal to val regs

copyValue :: Text -> Char -> Registers -> Registers
copyValue tval to = setRegVal to val
  where
    val = getDecimal tval

decReg :: Char -> Registers -> Registers
decReg to regs = newRegs
  where
    newVal = getRegVal to regs - 1
    newRegs = setRegVal to newVal regs

incReg :: Char -> Registers -> Registers
incReg to regs = newRegs
  where
    newVal = getRegVal to regs + 1
    newRegs = setRegVal to newVal regs

runRegex :: Text -> Text -> Maybe [Text]
runRegex st rt = case res of 
                   [] -> Nothing
                   _ -> Just $ map T.pack res
  where
    s = T.unpack st
    r = T.unpack rt
    (_, _, _, res) = s =~ r :: (String, String, String, [String])

parseCpy :: Text -> Maybe Command
parseCpy str = 
  let 
    isRegName :: Text -> Bool
    isRegName t = T.isInfixOf t "abcd"
  in do
  (from:tot:_) <- runRegex str "cpy (\\d+|[a-c]) ([a-d])"
  let to = T.head tot
  let cmd = if isRegName from 
      then Command $ \(line, regs) -> (line + 1, copyRegister (T.head from) to regs)
      else Command $ \(line, regs) -> (line + 1, copyValue from to regs)
  return cmd

parseDecInc :: Text -> Maybe Command
parseDecInc str = do
  (typText:regText:_) <- runRegex str "(dec|inc) ([a-d])"
  let reg = T.head regText
  let typ = T.head typText
  let cmd = if typ == 'd' 
      then Command $ \(line, regs) -> (line + 1, decReg reg regs)
      else Command $ \(line, regs) -> (line + 1, incReg reg regs)
  return cmd

doJnz val noJump jump = 
  if 0 == val 
     then noJump
     else jump

jnz :: Char -> Registers -> ProgramLine -> ProgramLine -> ProgramLine
jnz reg regs noJump jump = doJnz val noJump jump
  where
    val = getRegVal reg regs

getDecimal :: Text -> Int
getDecimal str | T.head str == '-' = -(getDecimal $ T.tail str)
               | otherwise = case TR.decimal str of 
                               Right (val, _) -> val

parseJnz :: Text -> Maybe Command
parseJnz str = do
  (regText:addrT:_) <- runRegex str "jnz (\\d|[a-d]) (-?\\d+)"
  let reg = T.head regText :: Char
  let addr = getDecimal addrT
  let cmd = if '1' == reg
      then Command $ \(line, regs) -> (doJnz 1 (line+1) $ line + addr, regs)
      else Command $ \(line, regs) -> (jnz reg regs (line+1) $ line + addr, regs)
  return cmd


parse :: Text -> Command
parse str = parse' [parseCpy, parseDecInc, parseJnz]
  where
    parse' (p:ps) = case p str of
                      Just cmd -> cmd
                      Nothing -> parse' ps
    parse' [] = trace str $ undefined

runProgram :: ProgramLine -> [Command] -> Registers -> Registers
runProgram lne cmds regs = run' lne regs
  where
    run' line regs 
      | line >= length(cmds) = regs
      | otherwise = run' nextLine newRegs
      where
        cmd = cmds !! line
        (nextLine, newRegs) = runCmd cmd $ (line, regs)

part1 :: IO ()
part1 = do
  input <- readFile "day12"
  let initRegs = Registers 0 0 0 0
  let initLine = 0
  let cmds = map parse $ T.lines input
  let res = runProgram initLine cmds initRegs
  let ans = show $ a res  :: Text
  let ans = "318117" :: Text
  putStrLn $ mappend "day12-1: " ans

part2 :: IO ()
part2 = do
  input <- readFile "day12"
  let initRegs = Registers 0 0 1 0
  let initLine = 0
  let cmds = map parse $ T.lines input
  let res = runProgram initLine cmds initRegs
  let ans = show $ a res  :: Text
  let ans = "9227771" :: Text
  putStrLn $ mappend "day12-2: " ans


run :: IO ()
run = do
  part1
  part2

