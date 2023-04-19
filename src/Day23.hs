{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Day23 (day23) where

import Data.List
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as Set
import MyLib hiding (Direction (..))

data Direction = North | South | West | East
  deriving (Eq, Show, Ord)

type Index = (Int, Int)

type Set = MultiSet

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
readInput = Set.fromMap . drawMap (\case '#' -> Just 1; _ -> Nothing) . lines

surrounds :: Set Index
surrounds = Set.fromList [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

toDir :: Direction -> (Index, Set Index)
toDir North = ((0, -1), Set.fromList [(x, -1) | x <- [-1 .. 1]])
toDir South = ((0, 1), Set.fromList [(x, 1) | x <- [-1 .. 1]])
toDir West = ((-1, 0), Set.fromList [(-1, x) | x <- [-1 .. 1]])
toDir East = ((1, 0), Set.fromList [(1, x) | x <- [-1 .. 1]])

proposeMove :: Set Index -> Direction -> Index -> Index
proposeMove s d i
  | Set.null (Set.intersection s (Set.map (+& i) surrounds)) = i
  | Set.null (Set.intersection s (Set.map (+& i) (snd (d' !! 0)))) = i +& fst (d' !! 0)
  | Set.null (Set.intersection s (Set.map (+& i) (snd (d' !! 1)))) = i +& fst (d' !! 1)
  | Set.null (Set.intersection s (Set.map (+& i) (snd (d' !! 2)))) = i +& fst (d' !! 2)
  | Set.null (Set.intersection s (Set.map (+& i) (snd (d' !! 3)))) = i +& fst (d' !! 3)
  | otherwise = i
  where
    d' = map toDir $ iterate succ d

step :: Set Index -> Int -> Set Index
step s n = restrictedMove
  where
    d = toEnum n
    draft = Set.map (proposeMove s d) s
    restrictedMove = Set.map (\x -> let x' = proposeMove s d x in if x' `Set.occur` draft > 1 then x else x') s

step' :: Int -> Set Index -> Int
step' n s = if s' == s then n + 1 else step' (n + 1) s'
  where
    s' = step s n

day23 :: IO ()
day23 = do
  input <- readInput <$> readFile "input23.txt"
  -- input <- readInput <$> readFile "test23.txt"
  putStrLn $ ("day23a: " ++) $ show $ length $ filter (== '.') $ concat $ drawGraph (\case Nothing -> '.'; Just 1 -> '#'; Just _ -> 'X') $ Set.toMap $ foldl' step input [0 .. 9]
  putStrLn $ ("day23a: " ++) $ show $ step' 0 input
