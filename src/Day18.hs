{-# LANGUAGE LambdaCase #-}
module Day18 (day18) where

import MyLib
import Data.List.Split
import Data.List
import GHC.Bits

test :: String
test = ".^^.^.^^^^"

input :: String
input = "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."

nextRow :: String -> String
nextRow x = map f x'
  where
    x' = divvy 3 1 $ '.' : x ++ "."
    f n = if n `elem` xs then '^' else '.'
    xs =
      [ "^^."
      , ".^^"
      , "^.."
      , "..^"
      ]

encode :: String -> (Int, Integer)
encode s = (length s, f 0 s)
  where
    f n [] = n
    f n (x : xs) = f (2 * n + (\case ; '^' -> 1 ; '.' -> 0) x) xs

-- "..^^." = 
nextRow' :: (Int, Integer) -> (Int, Integer)
nextRow' (l, n) = (l, f 0 0 (shiftL n 1))
  where
    n' = shiftL n 1
    xs = [ 1, 3, 4, 6 ]
    f :: Int -> Integer -> Integer -> Integer
    f x y z 
      | x >= l = y
      | otherwise = f (x + 1) (if (z `mod` 8) `elem` xs then setBit y x else y) (shiftR z 1)

decode :: (Int, Integer) -> String
decode (l, n) = f l "" n
  where
    f x y z
      | x <= 0 = y
      | otherwise = f (x - 1) ((if even z then '.' else '^') : y) (shiftR z 1)

day18 :: IO ()
day18 = do
  putStrLn $ ("day18a: " ++) $ show $ sum $ map (\(x, y) -> x - popCount y) $ take 40 $ iterate nextRow' $ encode input
  putStrLn $ ("day18b: " ++) $ show $ sum $ map (\(x, y) -> x - popCount y) $ take 400000 $ iterate nextRow' $ encode input
  -- putStrLn $ ("day18b: " ++) $ show $ firstRepeat $ iterate nextRow' $ encode input
