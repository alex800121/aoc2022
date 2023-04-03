module Day8 where

import MyLib
import Data.List
import Data.Char
import Data.Maybe

calcVisible :: [Int] -> [Bool]
calcVisible = snd . mapAccumL f (-1)
  where
    f tallest x = if x > tallest then (x, True) else (tallest, False)

calcView :: [Int] -> [Int]
calcView l = map (length . f) l'
  where
    l' = mapMaybe uncons $ tails l
    f (_, []) = []
    f (x, y : ys)
      | y >= x = [y]
      | otherwise = y : f (x, ys)

input' f g h input =
  h $ concat $
  foldl1' (zipWith (zipWith g)) $
  zipWith ($) [id, map reverse, transpose, transpose . map reverse ] $ 
  map (map f . ($ input)) [id, map reverse, transpose, map reverse . transpose]
  
day8 :: IO ()
day8 = do
  input <- map (map digitToInt) . lines <$> readFile "input8.txt"
  putStrLn $ ("day8a: " ++) $ show $ input' calcVisible (||) (length . filter id) input
  putStrLn $ ("day8b: " ++) $ show $ input' calcView (*) maximum input