{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day9 where

import Protolude
import qualified Data.String as S
import qualified Data.Text as T

eatGarbage :: S.String -> (S.String, Int)
eatGarbage = eg 0
  where
   eg :: Int -> S.String -> (S.String, Int)
   -- eg cnt [] = ([], cnt)
   eg cnt ('!':_:tl) = eg cnt tl
   eg cnt ('>':tl) = (tl, cnt)
   eg cnt (_:xs) = eg (cnt + 1) xs

clean :: Text -> (S.String, Int)
clean txt = res
  where
    s = T.unpack txt
    res = cl (s, 0) ""
    cl :: (S.String, Int) -> S.String -> (S.String, Int)
    cl ([], cnt) ack = (reverse ack, cnt)
    cl (('<':xs), cnt) ack = cl (s, cnt + c') ack
      where 
        (s,c') = eatGarbage xs
    cl ((x:xs), cnt) ack = cl (xs, cnt) (x:ack)

run1 :: S.String -> Int -> Int
run1 [] depth = 0
run1 (',':xs) depth = run1 xs depth
run1 ('{':xs) depth = depth + run1 xs (depth + 1)
run1 ('}':xs) depth = run1 xs (depth - 1)
  

solve1 :: Text -> Int
solve1 txt = let
  (h:_) = T.lines txt
  (ctxt,_) = clean h
  in
    run1 ctxt 1

solve2 :: Text -> Int
solve2 txt = let
  (h:_) = T.lines txt
  (_,c) = clean h
  in
   c 

