module Day1 (day1) where

import MyLib
import Data.List.Split
import Data.List

day1 :: IO ()
day1 = do
  elves <- map (map (read @Int) . lines) . splitOn "\n\n" <$> readFile "input1.txt"
  putStrLn $ ("day1a: " ++) $ show $ maximum $ map sum elves
  putStrLn $ ("day1b: " ++) $ show $ sum $ take 3 $ reverse $ sort $ map sum elves
