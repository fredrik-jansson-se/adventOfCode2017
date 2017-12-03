{-# LANGUAGE OverloadedStrings #-}

import Protolude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Day1 as Day1
import qualified Day2 as Day2

main :: IO ()
main = hspec $ do
  describe "AdventOfCode" $ do
    describe "Day1" $ do 
      it "can solve 1122" $ do
        Day1.solve("1122") `shouldBe` 3
      it "can solve 1111" $ do
        Day1.solve("1111") `shouldBe` 4
      it "can solve 1234" $ do
        Day1.solve("1234") `shouldBe` 0
      it "can solve 91212129" $ do
        Day1.solve("91212129") `shouldBe` 9
      it "can solve 1" $ do
        line <- TIO.readFile "day1.txt"
        Day1.solve(T.init line) `shouldBe` 1216

      it "can solve 1212" $ do
        Day1.solve2("1212") `shouldBe` 6
      it "can solve 1221" $ do
        Day1.solve2("1221") `shouldBe` 0
      it "can solve 123425" $ do
        Day1.solve2("123425") `shouldBe` 4
      it "can solve 123123" $ do
        Day1.solve2("123123") `shouldBe` 12
      it "can solve 12131415" $ do
        Day1.solve2("12131415") `shouldBe` 4
      it "can solve 1" $ do
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

