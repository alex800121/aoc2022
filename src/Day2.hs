{-# LANGUAGE LambdaCase #-}
module Day2 (day2) where

import MyLib
import Data.List (foldl', scanl')
import Data.Bifunctor
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

type Index = (Int, Int)
type NumPadPlus = Map Index Char

numPad :: [String]
numPad =
  [ "123"
  , "456"
  , "789"
  ]

numPadPlus :: NumPadPlus
numPadPlus = drawMap (\case ; ' ' -> Nothing; a -> Just a) $ lines inputS
  where
    inputS = "  1\n 234\n56789\n ABC\n  D"

testInput :: [String]
testInput = lines "ULL\nRRDDD\nLURDL\nUUUUD"

toNumPad :: Index -> Char
toNumPad (x, y) = numPad !! y !! x

step1 :: String -> (Int, Int) -> (Int, Int)
step1 s i = foldl' (flip f) i s
  where
    f 'U' = bimap g g . (+& (0, -1))
    f 'D' = bimap g g . (+& (0, 1))
    f 'R' = bimap g g . (+& (1, 0))
    f 'L' = bimap g g . (+& (-1, 0))
    g = max 0 . min 2

step2 :: NumPadPlus -> (Int, Int) -> String -> (Int, Int)
step2 npp = foldl' (flip f)
  where
    f x i = case npp Map.!? i' of
      Nothing -> i
      Just a -> i'
      where
        i' = case x of
          'U' -> i +& (0, -1)
          'D' -> i +& (0, 1)
          'R' -> i +& (1, 0)
          'L' -> i +& (-1, 0)

day2 :: IO ()
day2 = do
  input <- lines <$> readFile "input2.txt"
  putStrLn $ ("day2a: " ++) $ map toNumPad $ tail $ scanl' (flip step1) (1, 1) input
  putStrLn $ ("day2b: " ++) $ map (numPadPlus Map.!) $ tail $ scanl' (step2 numPadPlus) (0, 2) input
