module Day7 where

import Data.List (find, isInfixOf, tails)

split :: String -> ([String], [String])
split s = split' s ([], [])
  where
    split' :: String -> ([String], [String]) -> ([String], [String])
    split' [] (i,o) = (reverse i, reverse o)
    split' xs (supernet, hypernet) = split' newS (o:supernet, i:hypernet)
      where
        (o,s') = span (/='[') xs
        (i, s'') = span (/=']') $ dropWhile (=='[') s'
        newS = dropWhile (==']') s''

isAbba :: String -> Bool
isAbba (a:b:c:d:_) = (a==d) && (b==c) && (a/=b)
isAbba _ = False


containsAbba :: String -> Bool
containsAbba s = any isAbba tls
  where
    tls :: [String]
    tls = tails s

isValid :: ([String], [String]) -> Bool
isValid (supernet,hypernet) = (not i) &&  o
  where
    i = any containsAbba hypernet
    o = any containsAbba supernet

isAba :: String -> Bool
isAba (a:b:c:_) = (a==c) && (a/=b)
isAba _ = False

getAbas :: String -> [String]
getAbas = getAbas' []
  where
    getAbas' :: [String] -> String -> [String]
    getAbas' acc (a:tl@(b:c:_)) | a==c && (a /= b) = getAbas' (aba:acc) tl
                                | otherwise = getAbas' acc tl
                              where
                                aba :: String
                                aba = [a,b,a]
    getAbas' acc _ = acc

aba2bab :: String -> String
aba2bab (a:b:_) = [b,a,b]
aba2bab _ = ""

containsAba :: [String] -> String -> Bool
containsAba abas s = any (\aba -> isInfixOf aba s) abas

canTLS :: ([String], [String]) -> Bool
canTLS (supernet, hypernet) = res
  where
    abas = concat $ map getAbas supernet
    babs = map aba2bab abas
    log = show abas ++ " hypernet: " ++ show hypernet
    res = any (containsAba babs) hypernet

part1 :: IO ()
part1 = do
  input <- readFile "day7"
  let items = map split $ lines input
  let valids = filter isValid items
  let ans = show $ length valids
  putStrLn $ "day7-1: " ++ ans

part2 :: IO ()
part2 = do
  putStrLn $ show $ getAbas "ababcdc"
  input <- readFile "day7"
  -- let input = "aba[bab]xyz"
  -- let input = "xyx[xyx]xyx"
  -- let input = "aaa[kek]eke"
  -- let input = "zazbz[bzb]cdb"
  let items = map split $ lines input
  let valids = filter canTLS items
  let ans = show $ length valids
  putStrLn $ "day7-2: " ++ ans
  -- 42


run :: IO ()
run = do
  part1
  part2

