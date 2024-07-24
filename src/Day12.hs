{-# LANGUAGE GADTs #-}

module Day12 (day12) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib
import Paths_AOC2022

type Index = (Int, Int)

type Terrain = Map Index Char

adjacent :: [Index]
adjacent = [(1, 0), (-1, 0), (0, 1), (0, -1)]

reachable :: Terrain -> Index -> Index -> Bool
reachable m from to = fromMaybe False $ do
  from' <- m Map.!? from
  to' <- m Map.!? to
  return $ f from' to' && to `elem` map (from +&) adjacent
  where
    f 'S' y = f 'a' y
    f x 'E' = f x 'z'
    f x y = y <= succ x

bfs :: Terrain -> Set Index -> Set Index -> Set Index -> (Index -> Index -> Bool) -> Int -> Int
bfs t travelled ends starts test steps
  | not $ Set.disjoint ends starts = steps
  | otherwise = bfs t travelled' ends starts' test (steps + 1)
  where
    starts' = (Set.\\ travelled) $ Set.filter (`Map.member` t) $ Set.unions $ Set.map (\from -> Set.fromList $ filter (test from) $ map (+& from) adjacent) starts
    travelled' = Set.union starts travelled

day12 :: IO ()
day12 = do
  input <- drawMap Just . lines <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  -- input <- drawMap Just . lines <$> readFile "test12.txt"
  putStrLn $ ("day12a: " ++) $ show $ bfs input Set.empty (Map.keysSet $ Map.filter (== 'E') input) (Map.keysSet $ Map.filter (== 'S') input) (reachable input) 0
  putStrLn $ ("day12b: " ++) $ show $ bfs input Set.empty (Map.keysSet $ Map.filter ((||) <$> (== 'a') <*> (== 'S')) input) (Map.keysSet $ Map.filter (== 'E') input) (flip (reachable input)) 0
