module Day4 (day4) where

import Paths_AOC2022
import Data.List.Split
import Data.List
import Data.Tuple

contains :: Ord a => (a, a) -> (a, a) -> Bool
(x, y) `contains` (z, w) = x <= z && y >= w

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (x, y) (z, w) = (x <= z && y >= z) || (x <= w && y >= w)

day4 :: IO ()
day4 = do
  let f (x : y : _) = (x, y)
  input <- map (f . map (f . map (read @Int) . splitOn "-") . splitOn ",") . lines <$> (getDataDir >>= readFile . (++ "/input/input4.txt")) 
  putStrLn $ ("day4a: " ++) $ show $ length $ filter (\x -> uncurry contains x || uncurry contains (swap x)) input
  putStrLn $ ("day4b: " ++) $ show $ length $ filter (\x -> uncurry overlaps x || uncurry overlaps (swap x)) input
