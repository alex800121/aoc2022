module Day20 (day20) where

import MyLib
import Data.List.Split
import Data.List
import Data.Function

-- inputN :: Int
-- inputN = 4294967295
--
-- inputList :: Vec (S Z) (Int, Int)
-- inputList = Cons (0, inputN + 1) Nil

withinRange :: Ord a => Vec n (a, a) -> a -> Bool
withinRange Nil _ = True
withinRange (Cons (x, y) xs) a = x <= a && a < y && withinRange xs a

day20a :: Ord a => [Vec (S Z) (a, a)] -> a -> a
day20a [] x = x
day20a (y@(Cons (_, b) _) : ys) x = if withinRange y x then day20a ys b else day20a ys x

day20b :: (Num a, Ord a) => [Vec (S Z) (a, a)] -> (a, a) -> (a, a)
day20b [] n = n
day20b (y@(Cons (a, b) _) : ys) (c, d)
  | a > d = day20b ys (c + (a - d), b)
  | b > d = day20b ys (c, b)
  | otherwise = day20b ys (c, d)


day20 :: IO ()
day20 = do
  input <- sortBy (compare `on` (\(Cons (x, _) Nil) -> x)) . map ((\(x : y : _) -> Cons (x, y + 1) Nil) . map (read @Int) . splitOn "-") . lines <$> readFile "input20.txt"
  putStrLn $ ("day20a: " ++) $ show $ day20a input 0
  putStrLn $ ("day20b: " ++) $ show $ fst $ day20b input (0, 0)
