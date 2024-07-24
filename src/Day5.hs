{-# LANGUAGE OverloadedStrings #-}
module Day5 (day5) where

import Paths_AOC2022
import MyLib
import Data.List.Split
import Data.List
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

type Instruction = (Int, Int, Int)
type Crates = IntMap String

buildCrates :: String -> Crates
buildCrates s = IM.fromList $ map (\(x : xs) -> (digitToInt x, filter isAlpha xs)) $ filter (isDigit . head) $ transpose $ reverse $ lines s

buildIns :: String -> [Instruction]
buildIns = map ((\(_ : x : _ : y : _ : z : _) -> (read x, read y, read z)) . words) . lines

readInsFunc :: (String -> String) -> Crates -> Instruction -> Crates
readInsFunc f c (n, from, to) = IM.insert from fromCrate' $ IM.insert to toCrate' c
  where
    fromCrate = c IM.! from
    toCrate = c IM.! to
    (fromCrate', taken) = splitAt (length fromCrate - n) fromCrate
    toCrate' = toCrate <> f taken
    
readInsStack = readInsFunc reverse
readInsQueue = readInsFunc id
day5 :: IO ()
day5 = do
  x : y : _ <- splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input5.txt")) 
  let crates = buildCrates x
      instructions = buildIns y
  putStrLn $ ("day5a: " <>) $ map (last . snd) $ IM.toList $ foldl' readInsStack crates instructions
  putStrLn $ ("day5b: " <>) $ map (last . snd) $ IM.toList $ foldl' readInsQueue crates instructions
