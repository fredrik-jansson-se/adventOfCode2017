{-# LANGUAGE OverloadedStrings #-}

import Protolude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day7 as Day7
import qualified Day8 as Day8
import qualified Day9 as Day9
import qualified Day10 as Day10
import qualified Common as C
import qualified Day11 as Day11
import qualified Day12 as Day12
import qualified Day13 as Day13
import qualified Day14 as Day14
import qualified Day15 as Day15
import qualified Day16 as Day16
import qualified Day17 as Day17
import qualified Day18 as Day18
import qualified Day24 as Day24

main :: IO ()
main = hspec $ do
  describe "AdventOfCode" $ do
    -- describe "Common" $ do
    --   it "can parse csv" $ do
    --     C.csv "1,2,3, 5, 7" `shouldBe` [1,2,3,5,7]
    -- describe "Day1" $ do 
    --   it "solve 1" $ do
    --     Day1.solve("1122") `shouldBe` 3
    --     Day1.solve("1111") `shouldBe` 4
    --     Day1.solve("1234") `shouldBe` 0
    --     Day1.solve("91212129") `shouldBe` 9
    --     line <- TIO.readFile "day1.txt"
    --     Day1.solve(T.init line) `shouldBe` 1216

    --   it "can solve 2" $ do
    --     Day1.solve2("1212") `shouldBe` 6
    --     Day1.solve2("1221") `shouldBe` 0
    --     Day1.solve2("123425") `shouldBe` 4
    --     Day1.solve2("123123") `shouldBe` 12
    --     Day1.solve2("12131415") `shouldBe` 4
    --     line <- TIO.readFile "day1.txt"
    --     Day1.solve2(T.init line) `shouldBe` 1072
    -- describe "Day2" $ do 
    --   it "can solve example" $ do
    --     let txt = "5 1 9 5\n7 5 3\n2 4 6 8" :: Text
    --     Day2.solve1(txt) `shouldBe` 18
    --   it "can solve 1" $ do
    --     line <- TIO.readFile "day2.txt"
    --     Day2.solve1(T.init line) `shouldBe` 46402
    --   it "can solve example 2" $ do
    --     let txt = "5 9 2 8\n9 4 7 3\n3 8 6 5\n" :: Text
    --     Day2.solve2(txt) `shouldBe` 9
    --   it "can solve 2" $ do
    --     line <- TIO.readFile "day2.txt"
    --     Day2.solve2(T.init line) `shouldBe` 265
    -- describe "Day3" $ do 
    --   it "can solve 1" $ do
    --     Day3.solve1(1) `shouldBe` 0
    --     Day3.solve1(12) `shouldBe` 3
    --     Day3.solve1(23) `shouldBe` 2
    --     Day3.solve1(1024) `shouldBe` 31
    --     Day3.solve1(361527) `shouldBe` 326
    --   it "can solve 2" $ do
    --     Day3.solve2(805) `shouldBe` 806
    --     Day3.solve2(361527) `shouldBe` 363010
    -- describe "Day4" $ do
    --   it "can solve 1" $ do
    --     Day4.solve1("aa bb cc dd ee\n") `shouldBe` 1
    --     Day4.solve1("aa bb cc dd aa\n") `shouldBe` 0
    --     Day4.solve1("aa bb cc dd aaa\n") `shouldBe` 1
    --     file <- TIO.readFile "day4.txt"
    --     Day4.solve1(file) `shouldBe` 455
    --   it "can solve 2" $ do
    --     Day4.solve2("abcde fghij\n") `shouldBe` 1
    --     Day4.solve2("abcde xyz ecdab\n") `shouldBe` 0
    --     Day4.solve2("a ab abc abd abf abj\n") `shouldBe` 1
    --     Day4.solve2("iiii oiii ooii oooi oooo\n") `shouldBe` 1
    --     Day4.solve2("oiii ioii iioi iiio\n") `shouldBe` 0
    --     file <- TIO.readFile "day4.txt"
    --     Day4.solve2(file) `shouldBe` 186
    -- describe "Day5" $ do
    --   it "can solve 1" $ do
    --     Day5.solve1("0\n3\n0\n1\n-3") `shouldBe` 5
    --     file <- TIO.readFile "day5.txt"
    --     Day5.solve1(file) `shouldBe` 356945
    --   it "can solve 2" $ do
    --     Day5.solve2("0\n3\n0\n1\n-3") `shouldBe` 10
    --     file <- TIO.readFile "day5.txt"
    --     Day5.solve2(file) `shouldBe` 28372145
    -- describe "Day6" $ do
    --   it "can solve 1" $ do
    --     Day6.solve1("0 2 7 0") `shouldBe` 5
    --     file <- TIO.readFile "day6.txt"
    --     Day6.solve1(file) `shouldBe` 6681
    --   it "can solve 2" $ do
    --     Day6.solve2("0 2 7 0") `shouldBe` 4
    --     file <- TIO.readFile "day6.txt"
    --     Day6.solve2(file) `shouldBe` 2392
    -- describe "Day7" $ do
    --   it "can solve 1" $ do
    --     let v1 = Day7.solve1("pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n")
    --     v1 `shouldBe` "tknk"
    --     file <- TIO.readFile "day7.txt"
    --     Day7.solve1(file) `shouldBe` "hlqnsbe"
    --   it "can solve 2" $ do
    --     let v1 = Day7.solve2 "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n" "tknk"
    --     v1 `shouldBe` 60
    --     file <- TIO.readFile "day7.txt"
    --     Day7.solve2 file "hlqnsbe"  `shouldBe` 1993
    -- describe "Day8" $ do
    --   it "can solve 1" $ do
    --     let i = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
    --     Day8.solve1 i `shouldBe` 1
    --     file <- TIO.readFile "day8.txt"
    --     Day8.solve1 file `shouldBe` 3880
    --   it "can solve 2" $ do
    --     let i = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
    --     Day8.solve2 i `shouldBe` 10
    --     file <- TIO.readFile "day8.txt"
    --     Day8.solve2 file `shouldBe` 5035
    -- describe "Day9" $ do
    --   it "can eat garbage" $ do
    --     Day9.eatGarbage "<>" `shouldBe` ("", 1)
    --     Day9.eatGarbage "<random characters>" `shouldBe` ("", 18)
    --     Day9.eatGarbage "<<<<>" `shouldBe` ("", 4)
    --     Day9.eatGarbage "<{!>}>" `shouldBe` ("", 3)
    --     Day9.eatGarbage "<!!>" `shouldBe` ("", 1)
    --     Day9.eatGarbage "<!!!>>" `shouldBe` ("", 1)
    --     Day9.eatGarbage "<{o\"i!a,<{i<a>" `shouldBe` ("", 11)
    --   it "can solve 1" $ do
    --     Day9.solve1 "{}" `shouldBe` 1
    --     Day9.solve1 "{{{}}}" `shouldBe` 6
    --     Day9.solve1 "{{},{}}" `shouldBe` 5
    --     Day9.solve1 "{{{},{},{{}}}}" `shouldBe` 16
    --     Day9.solve1 "{<a>,<a>,<a>,<a>}" `shouldBe` 1
    --     Day9.solve1 "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
    --     Day9.solve1 "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
    --     Day9.solve1 "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
    --     file <- TIO.readFile "day9.txt"
    --     Day9.solve1 file `shouldBe` 9662
    --   it "can solve 1" $ do
    --     file <- TIO.readFile "day9.txt"
    --     Day9.solve2 file `shouldBe` 4903
    -- describe "Day10" $ do
    --   it "solve 1" $ do
    --     Day10.solve1 5 "3, 4, 1, 5" `shouldBe` 12
    --     file <- TIO.readFile "day10.txt"
    --     Day10.solve1 256 file `shouldBe` 1980
      -- it "solve 2" $ do
      --   Day10.solve2 256 "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      --   Day10.solve2 256 "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      --   Day10.solve2 256 "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      --   Day10.solve2 256 "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
      --   file <- TIO.readFile "day10.txt"
      --   Day10.solve2 256 file `shouldBe` "899124dac21012ebc32e2f4d11eaec55"
    -- describe "Day11" $ do
    --   it "can solve 1" $ do
    --     Day11.solve1 "ne,ne,ne" `shouldBe` 3
    --     Day11.solve1 "ne,ne,sw,sw" `shouldBe` 0
    --     Day11.solve1 "ne,ne,s,s" `shouldBe` 2
    --     Day11.solve1 "se,sw,se,sw,sw" `shouldBe` 3
    --     file <- TIO.readFile "day11.txt"
    --     Day11.solve1 file `shouldBe` 810
    --   it "can solve 1" $ do
    --     file <- TIO.readFile "day11.txt"
    --     Day11.solve2 file `shouldBe` 1567
    -- describe "Day12" $ do
    --   it "can parse" $ do
    --     Day12.parse "2 <-> 0, 3, 4\n" `shouldBe` [(2, [0,3,4])]
    --   it "can solve 1" $ do
    --     Day12.solve1 "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n" `shouldBe` 6
    --     file <- TIO.readFile "day12.txt"
    --     Day12.solve1 file `shouldBe` 378
    --   it "can solve 2" $ do
    --     Day12.solve2 "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n" `shouldBe` 2
    --     file <- TIO.readFile "day12.txt"
    --     Day12.solve2 file `shouldBe` 204
    -- describe "Day13" $ do
    --   it "can parse" $ do
    --     Day13.parse "0: 3\n1: 2\n4: 4\n6: 4\n" `shouldBe` [(0,3), (1,2), (4,4), (6,4)]
    --   it "can solve 1" $ do
    --     Day13.solve1 "0: 3\n1: 2\n4: 4\n6: 4\n" `shouldBe` 24
    --     file <- TIO.readFile "day13.txt"
    --     Day13.solve1 file `shouldBe` 1840
    --   it "can solve 2" $ do
    --     Day13.solve2 "0: 3\n1: 2\n4: 4\n6: 4\n" `shouldBe` 10
    --     file <- TIO.readFile "day13.txt"
    --     Day13.solve2 file `shouldBe` 3850260
    -- describe "Day14" $ do
    --   it "can solve 1" $ do
    --     Day14.solve1 "flqrgnkx" `shouldBe` 8108
    --     Day14.solve1 "ugkiagan" `shouldBe` 8292
    --   it "can solve 2" $ do
    --     Day14.solve2 "flqrgnkx" `shouldBe` 1242
    --     Day14.solve2 "ugkiagan" `shouldBe` 1069
    -- describe "Day15" $ do
    --   it "can solve 1" $ do
    --     Day15.solve1 65 8921 `shouldBe` 588
    --     Day15.solve1 289 629 `shouldBe` 638
    --   it "can solve 2" $ do
    --     Day15.solve2 65 8921 `shouldBe` 309
    --     Day15.solve2 289 629 `shouldBe` 343
    -- describe "Day16" $ do
    --   it "can solve 1" $ do
    --     v1 <- Day16.solve1 "s1,x3/4,pe/b" "abcde" 
    --     v1 `shouldBe` "baedc"
    --     file <- TIO.readFile "day16.txt"
    --     v2 <- Day16.solve1 file "abcdefghijklmnop" 
    --     v2 `shouldBe` "dcmlhejnifpokgba"
    --   it "can solve 2" $ do
    --     Day16.solve2 "s1,x3/4,pe/b" "abcde" 2 `shouldBe` "ceadb"
    --     file <- TIO.readFile "day16.txt"
    --     Day16.solve2 file "abcdefghijklmnop" 1000000000 `shouldBe` "dcmlhejnifpokgba"
    -- describe "Day17" $ do
    --   it "can solve 1" $ do
    --     Day17.solve1 3 `shouldBe` 638
    --     Day17.solve1 343 `shouldBe` 1914
    --   it "can solve 2" $ do
    --     Day17.solve2 343 `shouldBe` 41797835
    describe "Day18" $ do
      it "can solve 1" $ do
        Day18.solve1 "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2" `shouldBe` 4
        file <- TIO.readFile "day18.txt"
        Day18.solve1 file `shouldBe` 1187
    -- describe "Day24" $ do
    --   it "can solve 1" $ do
    --     Day24.solve1 "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10" `shouldBe` 31
    --     file <- TIO.readFile "day24.txt"
    --     Day24.solve1 file `shouldBe` 1914
