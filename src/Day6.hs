module Day6 (day6) where

import Data.List
import Data.Function
day6 :: IO ()
day6 = do
  input <- lines <$> readFile "input6.txt"
  -- putStrLn $ ("day6a: " ++) $ show $ map head $ sortBy (compare `on` length) $ map (group . sort) $ transpose input
  putStrLn $ ("day6a: " ++) $ map (head . maximumBy (compare `on` length) . group . sort) $ transpose input
  putStrLn $ ("day6b: " ++) $ map (head . minimumBy (compare `on` length) . group . sort) $ transpose input
