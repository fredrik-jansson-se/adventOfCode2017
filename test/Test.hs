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

main :: IO ()
main = hspec $ do
  describe "AdventOfCode" $ do
    describe "Day1" $ do 
      it "solve 1" $ do
        Day1.solve("1122") `shouldBe` 3
        Day1.solve("1111") `shouldBe` 4
        Day1.solve("1234") `shouldBe` 0
        Day1.solve("91212129") `shouldBe` 9
        line <- TIO.readFile "day1.txt"
        Day1.solve(T.init line) `shouldBe` 1216

      it "can solve 2" $ do
        Day1.solve2("1212") `shouldBe` 6
        Day1.solve2("1221") `shouldBe` 0
        Day1.solve2("123425") `shouldBe` 4
        Day1.solve2("123123") `shouldBe` 12
        Day1.solve2("12131415") `shouldBe` 4
        line <- TIO.readFile "day1.txt"
        Day1.solve2(T.init line) `shouldBe` 1072
    describe "Day2" $ do 
      it "can solve example" $ do
        let txt = "5 1 9 5\n7 5 3\n2 4 6 8" :: Text
        Day2.solve1(txt) `shouldBe` 18
      it "can solve 1" $ do
        line <- TIO.readFile "day2.txt"
        Day2.solve1(T.init line) `shouldBe` 46402
      it "can solve example 2" $ do
        let txt = "5 9 2 8\n9 4 7 3\n3 8 6 5\n" :: Text
        Day2.solve2(txt) `shouldBe` 9
      it "can solve 2" $ do
        line <- TIO.readFile "day2.txt"
        Day2.solve2(T.init line) `shouldBe` 265
    describe "Day3" $ do 
      it "can solve 1" $ do
        Day3.solve1(1) `shouldBe` 0
        Day3.solve1(12) `shouldBe` 3
        Day3.solve1(23) `shouldBe` 2
        Day3.solve1(1024) `shouldBe` 31
        Day3.solve1(361527) `shouldBe` 326
      it "can solve 2" $ do
        Day3.solve2(805) `shouldBe` 806
        Day3.solve2(361527) `shouldBe` 363010
    describe "Day4" $ do
      it "can solve 1" $ do
        Day4.solve1("aa bb cc dd ee\n") `shouldBe` 1
        Day4.solve1("aa bb cc dd aa\n") `shouldBe` 0
        Day4.solve1("aa bb cc dd aaa\n") `shouldBe` 1
        file <- TIO.readFile "day4.txt"
        Day4.solve1(file) `shouldBe` 455
      it "can solve 2" $ do
        Day4.solve2("abcde fghij\n") `shouldBe` 1
        Day4.solve2("abcde xyz ecdab\n") `shouldBe` 0
        Day4.solve2("a ab abc abd abf abj\n") `shouldBe` 1
        Day4.solve2("iiii oiii ooii oooi oooo\n") `shouldBe` 1
        Day4.solve2("oiii ioii iioi iiio\n") `shouldBe` 0
        file <- TIO.readFile "day4.txt"
        Day4.solve2(file) `shouldBe` 186
    describe "Day5" $ do
      it "can solve 1" $ do
        Day5.solve1("0\n3\n0\n1\n-3") `shouldBe` 5
        -- file <- TIO.readFile "day5.txt"
        -- Day5.solve1(file) `shouldBe` 356945
      it "can solve 2" $ do
        Day5.solve2("0\n3\n0\n1\n-3") `shouldBe` 10
        -- file <- TIO.readFile "day5.txt"
        -- Day5.solve2(file) `shouldBe` 28372145
    describe "Day6" $ do
      it "can solve 1" $ do
        Day6.solve1("0 2 7 0") `shouldBe` 5
        -- file <- TIO.readFile "day6.txt"
        -- Day6.solve1(file) `shouldBe` 6681
      it "can solve 2" $ do
        Day6.solve2("0 2 7 0") `shouldBe` 4
        -- file <- TIO.readFile "day6.txt"
        -- Day6.solve2(file) `shouldBe` 2392
    describe "Day7" $ do
      it "can solve 1" $ do
        let v1 = Day7.solve1("pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n")
        v1 `shouldBe` "tknk"
        file <- TIO.readFile "day7.txt"
        Day7.solve1(file) `shouldBe` "hlqnsbe"
      it "can solve 2" $ do
        let v1 = Day7.solve2 "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n" "tknk"
        v1 `shouldBe` 60
        file <- TIO.readFile "day7.txt"
        Day7.solve2 file "hlqnsbe"  `shouldBe` 1993
    describe "Day8" $ do
      it "can solve 1" $ do
        let i = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
        Day8.solve1 i `shouldBe` 1
        file <- TIO.readFile "day8.txt"
        Day8.solve1 file `shouldBe` 3880
      it "can solve 2" $ do
        let i = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
        Day8.solve2 i `shouldBe` 10
        file <- TIO.readFile "day8.txt"
        Day8.solve2 file `shouldBe` 5035
