module Day25 (day25) where

import MyLib

type SNAFU = String

fromSNAFU :: String -> Integer
fromSNAFU = f 0
  where
    f acc [] = acc
    f acc ('2' : xs) = f (acc * 5 + 2) xs
    f acc ('1' : xs) = f (acc * 5 + 1) xs
    f acc ('0' : xs) = f (acc * 5 + 0) xs
    f acc ('-' : xs) = f (acc * 5 - 1) xs
    f acc ('=' : xs) = f (acc * 5 - 2) xs

toSNAFU :: Integer -> String
toSNAFU 0 = "0"
toSNAFU x = f "" x
  where
    f acc 0 = acc
    f acc x = case x `mod` 5 of
      0 -> f ('0' : acc) (x `div` 5)
      1 -> f ('1' : acc) (x `div` 5)
      2 -> f ('2' : acc) (x `div` 5)
      3 -> f ('=' : acc) ((x `div` 5) + 1)
      4 -> f ('-' : acc) ((x `div` 5) + 1)

day25 :: IO ()
day25 = do
  -- input <- map fromSNAFU . lines <$> readFile "test25.txt"
  input <- map fromSNAFU . lines <$> readFile "input25.txt"
  putStrLn $ ("day25a: " ++) $ toSNAFU $ sum input
