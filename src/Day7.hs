{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day7 where
import Protolude
import Common
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List (partition)

parseHeader :: Text -> (Text, Int)
parseHeader txt = let
  (h,v) = T.breakOn " " txt
  val = T.dropAround (\c -> c `elem` [' ', '(', ')']) v
  in
    (h, parseInt 0 val)

parse :: Text -> ((Text, Int), [Text]) 
parse txt = case T.splitOn "->" txt of
  (h:[]) -> (parseHeader h, [])
  (hdr:rest:_) -> (parseHeader hdr, map T.strip $ T.splitOn "," rest)

type MM = M.Map Text Text

addToMap :: MM -> ((Text, Int), [Text]) -> MM
addToMap m ((hdr, _) , xs) = foldl (\m' w -> M.insert w hdr m') m xs

run1 word m = case M.lookup word m of
  Just word' -> run1 word' m
  Nothing -> word

solve1 :: Text -> Text
solve1 txt = let
  ll = T.lines txt
  m = map parse ll
  m' = foldl' addToMap M.empty m
  (k: _) = M.keys m'
  in
    run1 k m'

type MI = M.Map Text Int

type MC = M.Map Text [Text]

data Tree = Fork Text Int [Tree]
          | Leaf Text Int
        deriving (Show)

toTree :: Text -> MI -> MC -> Tree
toTree cur weights lu = 
  case lu M.! cur of
    [] -> Leaf cur weight
    chlds -> Fork cur weight $ map (\c -> toTree c weights lu) chlds
    where
      weight = weights M.! cur
    

buildMaps :: Text -> [((Text, Int), [Text])] -> (Tree, MI)
buildMaps start input = let
  (m, wts) = foldl' (\(m,i) ((name, weight), chlds) -> (M.insert name chlds m, M.insert name weight i)) (M.empty :: MC, M.empty :: MI) input
  tree = toTree start wts m
  in
    (tree, wts)

weight :: Tree -> Int
weight (Leaf _ w) = w
weight (Fork _ w chlds) = w + (sum $ map weight chlds)

isBalanced :: Tree -> Bool
isBalanced (Leaf _ _) = True
isBalanced (Fork _ _ chlds) = and $ map (== h) wts
  where
    (h:wts) = map weight chlds

findUnbalanced :: Tree -> Tree
findUnbalanced t@(Fork _ _ c) =
  if (not tib) && cib then t else findUnbalanced s
  where
    tib = isBalanced t
    cib = all isBalanced c
    (s:ss) = filter (\x -> not $ isBalanced x) c

expand :: Tree -> [(Int, Int)]
expand (Fork _ _ clds) = map (\(Fork _ w c) -> (w, w + (sum $ map weight c))) clds

part :: [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
part a@(x:xs) = partition (\(_, s) -> s == snd x) a

getDifferent :: ([(Int, Int)], [(Int, Int)]) -> Int
getDifferent (a, b) = if length a == 1 then gd a b 
                                   else gd b a
  where gd ((aw, as):[]) ((_, bs):_) = aw + (bs - as)

-- 2448, wrong
-- 2438
--
solve2 :: Text -> Text -> Int
solve2 txt topNode = let
  ll = T.lines txt
  m = map parse ll
  (t, wts) = buildMaps topNode m
  unb@(Fork unb_name _ c) = findUnbalanced t
  cc = map weight c
  e = expand unb
  p = part e
  gd = getDifferent p
  in
    gd
