module Day9 (day9) where

import Data.List
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib
import Paths_AOC2022

data Dir
  = R
  | L
  | U
  | D
  deriving (Show, Read, Eq, Ord)

type Index = (Int, Int)

type Rope = [Index]

type Ins = (Dir, Int)

step :: (Set Index, Rope) -> Ins -> (Set Index, Rope)
step (s, []) _ = (s, [])
step (s, x : xs) (d, n)
  | n <= 0 = (s, x : xs)
  | otherwise = step ((s, xs, id) `follow` move x d) (d, n - 1)

move :: Index -> Dir -> Index
move i d = i +& i'
  where
    i' = case d of
      R -> (1, 0)
      L -> (-1, 0)
      U -> (0, 1)
      D -> (0, -1)

-- follow :: Rope -> Index -> Rope
follow (s, [], f) x = (Set.insert x s, f [x])
follow (s, y : ys, f) x = follow (s, ys, f . (x :)) y'
  where
    y' = y `follows` x

follows :: Index -> Index -> Index
follows y@(a, b) x@(c, d)
  | y `surrounds` x = y
  | otherwise = y +& (signum (c - a), signum (d - b))

surrounds :: Index -> Index -> Bool
surrounds x y = x `elem` [y +& (a, b) | a <- [(-1) .. 1], b <- [(-1) .. 1]]

day9 :: IO ()
day9 = do
  input <- concatMap ((\(x, y) -> replicate y (x, 1)) . (\(x : y : _) -> (read @Dir x, read @Int y)) . words) . lines <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  putStrLn $ ("day9a: " ++) $ show $ length $ fst $ foldl' step (Set.empty, replicate 2 (0, 0)) input
  putStrLn $ ("day9b: " ++) $ show $ length $ fst $ foldl' step (Set.empty, replicate 10 (0, 0)) input
