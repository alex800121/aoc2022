{-# LANGUAGE LambdaCase #-}

module Day23 (day23) where

import Data.List
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib hiding (Direction (..))
import Paths_AOC2022

data Direction = North | South | West | East
  deriving (Eq, Show, Ord)

type Index = (Int, Int)

-- type Set = MultiSet

instance Enum Direction where
  fromEnum North = 0
  fromEnum South = 1
  fromEnum West = 2
  fromEnum East = 3
  toEnum n = case n `mod` 4 of
    0 -> North
    1 -> South
    2 -> West
    3 -> East

readInput :: String -> Set Index
readInput = Map.keysSet . drawMap (\case '#' -> Just 1; _ -> Nothing) . lines

surrounds :: Set Index
surrounds = Set.fromList [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

toDir :: Direction -> [Index]
toDir = \case
  North -> ([(x, -1) | x <- ls])
  South -> ([(x, 1) | x <- ls])
  West -> ([(-1, x) | x <- ls])
  East -> ([(1, x) | x <- ls])
  where
    ls = [0, -1, 1]

proposeMove :: Set Index -> Direction -> Index -> Index
proposeMove s d i
  | Set.null (Set.intersection s (Set.map (+& i) surrounds)) = i
  | Just x <- find (not . any (`Set.member` s)) d' = head x
  | otherwise = i
  where
    d' = map (map (i +&) . toDir) $ take 4 $ iterate succ d

step :: Set Index -> Int -> Set Index
step s n = restrictedMove
  where
    d = toEnum n
    draft = Set.foldl' (\m k -> Map.insertWith (<>) (proposeMove s d k) [k] m) Map.empty s
    restrictedMove = Map.foldlWithKey' f Set.empty draft
    f acc next [o] = Set.insert next acc
    f acc next os = acc <> Set.fromList os

step' :: Int -> Set Index -> Int
step' n s = if s' == s then n + 1 else step' (n + 1) s'
  where
    s' = step s n

day23 :: IO ()
day23 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  -- input <- readInput <$> (getDataDir >>= readFile . (++ "/input/test23.txt"))
  putStrLn $ ("day23a: " ++) $ show $ length $ filter (== '.') $ concat $ drawGraph (\case Nothing -> '.'; Just _ -> '#') $ Map.fromSet (const ()) $ foldl' step input [0 .. 9]
  putStrLn $ ("day23b: " ++) $ show $ step' 0 input

