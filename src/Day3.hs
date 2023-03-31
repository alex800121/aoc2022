module Day3 (day3) where

import MyLib
import Data.List
import Data.List.Split

isTriangle :: (Ord a, Num a) => [a] -> Bool
isTriangle l
  | length l /= 3 = False
  | otherwise = let [x, y, z] = sort l in x + y > z

day3 :: IO ()
day3 = do
  input <- map (map (read @Int) . words) . lines <$> readFile "input3.txt"
  putStrLn $ ("day3a: " ++) $ show $ length $ filter isTriangle input
  putStrLn $ ("day3b: " ++) $ show $ length $ filter isTriangle $ chunksOf 3 $ concat $ transpose input
