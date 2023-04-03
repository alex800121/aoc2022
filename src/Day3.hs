module Day3 (day3) where

import MyLib
import Data.Char
import Data.List
import Data.List.Split

prioritize :: Char -> Int
prioritize x
  | isUpper x = ord x - 38
  | isLower x = ord x - 96
  
day3 :: IO ()
day3 = do
  input' <- lines <$> readFile "input3.txt"
  let input = map (\x -> let n = length x `div` 2 in splitAt n x) input'
      input2 = map (foldr1 intersect) $ chunksOf 3 input'
  putStrLn $ ("day3a: " ++) $ show $ sum $ map (prioritize . head . uncurry intersect) input
  putStrLn $ ("day3b: " ++) $ show $ sum $ map (prioritize . head) input2