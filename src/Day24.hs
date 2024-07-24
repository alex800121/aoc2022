{-# LANGUAGE LambdaCase #-}

module Day24 (day24) where

import Paths_AOC2022
import Data.Array
import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import MyLib

type Blizzard = Array Index Char

type Index = (Int, Int)

type Bound = (Index, Index)

data GameState = G {time :: Int, start :: Index, end :: [Index]}
  deriving (Show, Eq)

instance Ord GameState where
  compare (G t1 s1 e1) (G t2 s2 e2) = compare (t1 + m1) (t2 + m2) <> compare t1 t2 <> compare m1 m2 <> compare s1 s2 <> compare e1 e2
    where
      f _ [] = 0
      f a (x : y) = manhattan' a x + f x y
      m1 = f s1 e1
      m2 = f s2 e2

manhattan' (a, b) (c, d) = abs (a - c) + abs (b - d)

inputParser :: String -> Blizzard
inputParser = (\x -> listArray ((0, 0), (length (head x) - 1, length x - 1)) (concat $ transpose x)) . map (init . tail) . init . tail . lines

adjacents :: Set Index
adjacents = Set.fromList [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]

checkAvail :: Bound -> Blizzard -> Int -> Index -> Bool
checkAvail b@((minx, miny), upperBound@(maxx, maxy)) blizzard minute a@(x, y) =
  a == (minx, miny - 1) || a == (maxx, maxy + 1) || (withinBound b a && all f "<>^v")
  where
    width = maxx - minx + 1
    height = maxy - miny + 1
    f c = case c of
      '>' -> (blizzard ! ((x - minute) `mod` width, y)) /= '>'
      '<' -> (blizzard ! ((x + minute) `mod` width, y)) /= '<'
      'v' -> (blizzard ! (x, (y - minute) `mod` height)) /= 'v'
      '^' -> (blizzard ! (x, (y + minute) `mod` height)) /= '^'

withinBound :: Bound -> Index -> Bool
withinBound ((a, b), (c, d)) (x, y) = x >= a && y >= b && x <= c && y <= d

choices :: Bound -> Blizzard -> Int -> Index -> Set Index
choices bound blizzard minute i = Set.filter (checkAvail bound blizzard minute) $ Set.map (+& i) adjacents

aStar :: Bound -> Blizzard -> Set GameState -> GameState
aStar bound blizzard s
  | null e = picked
  | head e == st = aStar bound blizzard $ Set.insert (G t (head e) (tail e)) rest
  | otherwise = aStar bound blizzard nextGs
  where
    (picked@(G t st e), rest) = Set.deleteFindMin s
    nexts = choices bound blizzard (t + 1) st
    nextGs = Set.union rest $ Set.map (\x -> G (t + 1) x e) nexts

day24 :: IO ()
day24 = do
  -- blizzard <- inputParser <$> readFile "test24.txt"
  blizzard <- inputParser <$>(getDataDir >>= readFile . (++ "/input/input24.txt")) 
  let bound@((mx, my), (w, h)) = bounds blizzard
      start = (mx, my - 1)
      end = (w, h + 1)
  -- print blizzard
  putStrLn $ ("day24a: " ++) $ show $ time $ aStar bound blizzard (Set.singleton (G 0 start [end]))
  putStrLn $ ("day24b: " ++) $ show $ time $ aStar bound blizzard (Set.singleton (G 0 start [end, start, end]))
