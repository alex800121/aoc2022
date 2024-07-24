module Day10 (day10) where

import Data.List
import Data.List.Split
import MyLib
import Paths_AOC2022

data Instruction
  = Noop
  | Addx Int
  deriving (Show, Ord, Eq)

readIns :: String -> Instruction
readIns = f . words
  where
    f ["noop"] = Noop
    f ["addx", y] = Addx $ read y

day10a :: [Instruction] -> Int -> [Int]
day10a [] _ = []
day10a (Noop : ins) x = x : day10a ins x
day10a (Addx i : ins) x = x : x : day10a ins (x + i)

draw :: (Int, Int) -> Char
draw (x, y) = if abs (x - y) <= 1 then '#' else ' '

day10 :: IO ()
day10 = do
  ins <- map readIns . lines <$> (getDataDir >>= readFile . (++ "/input/input10.txt"))
  let pos = day10a ins 1
      day10b = unlines $ map (zipWith (curry draw) [0 ..]) $ take 8 $ chunksOf 40 pos
  putStrLn $ ("day10a: " ++) $ show $ sum $ map (\x -> (* x) $ (pos !!) $ subtract 1 x) [20, 60, 100, 140, 180, 220]
  putStrLn ("day10b: \n" ++ day10b)
