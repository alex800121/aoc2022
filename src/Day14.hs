module Day14 (day14) where

import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import MyLib
import Paths_AOC2022

type Index = (Int, Int)

type Cave = Set Index

origin :: Index
origin = (500, 0)

buildCave :: [Index] -> Set Index
buildCave [] = Set.empty
buildCave [x] = Set.singleton x
buildCave (x : y : xs) = let a = bimap signum signum $ y -& x in Set.union (Set.fromList $ takeWhile (/= y) $ iterate (+& a) x) $ buildCave (y : xs)

dropSand :: Int -> Cave -> Index -> Index
dropSand bottom c (x, y)
  | y >= bottom = (x, y)
  | (x, y + 1) `Set.notMember` c = dropSand bottom c (x, y + 1)
  | (x - 1, y + 1) `Set.notMember` c = dropSand bottom c (x - 1, y + 1)
  | (x + 1, y + 1) `Set.notMember` c = dropSand bottom c (x + 1, y + 1)
  | otherwise = (x, y)

settleSand :: Int -> Index -> (Index, Cave) -> (Index, Cave)
-- settleSand bottom x (_, c) = trace (show y) (y, Set.insert y c)
settleSand bottom x (_, c) = (y, Set.insert (dropSand bottom c x) c)
  where
    y = dropSand bottom c x

day14 :: IO ()
day14 = do
  cave <- Set.unions . map (buildCave . map ((\(x : y : _) -> (read x, read y)) . splitOn ",") . splitOn " -> ") . lines <$> (getDataDir >>= readFile . (++ "/input/input14.txt"))
  -- cave <- Set.unions . map (buildCave . map ((\(x : y : _) -> (read x, read y)) . splitOn ",") . splitOn " -> ") . lines <$> readFile "test14.txt"
  let bottom = maximum $ Set.map snd cave
  putStrLn $ ("day14a: " ++) $ show $ subtract 1 $ fromJust $ findIndex ((>= bottom) . snd . fst) $ iterate (settleSand bottom origin) (origin, cave)
  -- print cave
  -- print bottom
  putStrLn $ ("day14b: " ++) $ show $ (+ 1) $ fromJust $ findIndex ((== origin) . fst) $ tail $ iterate (settleSand (bottom + 1) origin) (origin, cave)
