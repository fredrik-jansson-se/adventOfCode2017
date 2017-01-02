{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Astar where

import Protolude
import qualified Data.Set as S
import Data.Map.Strict ((!), lookup, empty, insert, singleton)
import Data.List (head, tail)

data AStarFns node a = AStarFns { 
  getCostEstimate :: node -> a,
  getDistance :: node -> node -> a,
  getNeighbors :: node -> [node] 
                                }

astar :: forall score node.(Num score, Ord score, Eq node, Ord node, Show node) => 
  AStarFns node score -> 
  node -> 
  node -> 
  Maybe [node]
astar fns start goal = 
  astar_int csInit osInit cfInit gsInit fsInit
  where
    csInit :: S.Set node
    csInit = S.empty

    fsInit :: Map node score
    fsInit = singleton start $ getCostEstimate fns start

    osInit :: [node]
    osInit = updateOpenSet start fsInit []

    cfInit :: Map node node
    cfInit = Data.Map.Strict.empty

    zeroCost :: score
    zeroCost = getCostEstimate fns goal

    gsInit :: Map node score 
    gsInit = singleton start zeroCost

    updateOpenSet :: node -> Map node score -> [node] -> [node]
    updateOpenSet n fScore os = sortBy sfn (n:os)
      where
        sfn n1 n2 = compare (fScore ! n1) (fScore ! n2)
  
    astar_int ::
      S.Set node ->
      [node] ->
      Map node node ->
      Map node score ->
      Map node score ->
      Maybe [node]
    astar_int _ [] _ _ _ = Nothing
    astar_int closedSet (current:os) cameFrom gScore fScore 
      | current == goal = Just $ reconstruct_path cameFrom start current
      | otherwise = astar_int newCS newOS newCF newGS newFS
        where
          newCS = S.insert current closedSet
          (newOS, newCF, newGS, newFS) = handle_neighbor current newCS os cameFrom gScore fScore $ getNeighbors fns current
          log = "astar_int, current = " ++ show cameFrom
          log2 = "pos = " ++ show current

    
    handle_neighbor :: 
      node -> 
      S.Set node ->
      [node] ->
       Map node node ->
       Map node score ->
       Map node score ->
       [node] ->
       ([node], Map node node, Map node score, Map node score)
    handle_neighbor current closedSet openSet cameFrom gScore fScore nbrs
      | null nbrs = (openSet, cameFrom, gScore, fScore)
      | S.member n closedSet = handle_neighbor current closedSet openSet cameFrom gScore fScore $ tail nbrs
      | not $ n `elem` openSet = handle_neighbor current closedSet newOS newCF newGS newFS $ tail nbrs
      | otherwise = handle_neighbor current closedSet openSet cameFrom gScore fScore $ tail nbrs
        where
          n :: node
          n = Data.List.head nbrs 
          tentative_gScore :: score
          tentative_gScore = gScore ! current + getDistance fns current n
          newCF = insert n current cameFrom
          log  = "Adding " ++ show n ++ " to CF"
          newGS = insert n tentative_gScore gScore
          costEst = getCostEstimate fns n
          newFS = insert n (tentative_gScore + costEst) fScore
          newOS = updateOpenSet n newFS openSet



  --       for each neighbor of current
  --           if neighbor in closedSet
  --               continue		// Ignore the neighbor which is already evaluated.
  --           // The distance from start to a neighbor
  --           tentative_gScore := gScore[current] + dist_between(current, neighbor)
  --           if neighbor not in openSet	// Discover a new node
  --               openSet.Add(neighbor)
  --           else if tentative_gScore >= gScore[neighbor]
  --               continue		// This is not a better path.

  --           // This path is the best until now. Record it!
  --           gScore[neighbor] := tentative_gScore
  --           fScore[neighbor] := gScore[neighbor] + heuristic_cost_estimate(neighbor, goal)

  --   return failure
    reconstruct_path :: Map node node -> node -> node -> [node]
    reconstruct_path cameFrom start goal = recp goal []
      where
        recp :: node -> [node] -> [node]
        recp cur acc 
            | cur == start = reverse acc
            | otherwise = recp next newAcc
          where
            next = cameFrom ! cur
            newAcc = cur:acc
            log = "current: " ++ show cur ++ " - next: " ++ show next

-- function reconstruct_path(cameFrom, current)
  --   total_path := [current]
  --   while current in cameFrom.Keys:
  --       current := cameFrom[current]
  --       total_path.append(current)
  --   return total_path
