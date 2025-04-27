module Day14 (day14) where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List ( foldl', findIndex )
import Data.List.Split ( splitOn )
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace ()
import MyLib ( (+&), (-&) )
import Paths_AOC2022 ( getDataDir )

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
settleSand bottom x (_, c) = (y, Set.insert (dropSand bottom c x) c)
  where
    y = dropSand bottom c x

solveB cave base y xs acc
  | y > base = acc
  | otherwise = solveB cave base (y + 1) xs' acc'
  where
    xs' = IS.foldl' f IS.empty xs
    f acc x = foldl' g acc [x - 1, x, x + 1]
    g acc x
      | (x, y + 1) `Set.notMember` cave = IS.insert x acc
      | otherwise = acc
    acc' = acc + IS.size xs

day14 :: IO ()
day14 = do
  cave <-
    Set.unions
      . map (buildCave . map ((\(x : y : _) -> (read x, read y)) . splitOn ",") . splitOn " -> ")
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input14.txt"))
  let bottom = maximum $ Set.map snd cave
      base = bottom + 1
  putStrLn
    . ("day14a: " ++)
    . show
    . subtract 1
    . fromJust
    . findIndex ((>= bottom) . snd . fst)
    $ iterate (settleSand bottom origin) (origin, cave)
  putStrLn
    . ("day14b: " ++)
    . show
    $ solveB cave base 0 (IS.singleton 500) 0
