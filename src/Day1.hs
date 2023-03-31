{-# LANGUAGE LambdaCase #-}
module Day1 (day1) where

import Data.List.Split
import MyLib
import Data.List (foldl', scanl')
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)

data Ins = I Turn Int deriving (Show, Eq, Ord)
data Turn = R | L deriving (Show, Eq, Ord)
type Index = (Int, Int)
data Bot = B { dir :: Direction, index :: Index } deriving Show

manhattan' :: Num a => (a, a) -> a
manhattan' (x, y) = abs x + abs y

parseIns :: String -> Ins
parseIns (x : xs) = I ((\case ; 'R' -> R ; 'L' -> L;) x) (read xs)

step :: Ins -> Bot -> Bot
step (I t n) = forwardN n . turn t

(-&) :: Num a => (a, a) -> (a, a) -> (a, a)
(x, y) -& (z, w) = (x - z, y - w)

step' :: Bot -> [Ins] -> [Bot]
step' b [] = [b]
step' b1@(B d1 i1) (x : xs) = 
  [ B d1 il 
  | il <- takeWhile (/= i2) $
          iterate (+& bimap signum signum (i2 -& i1)) i1
  ] ++ step' b2 xs
  where
    b2@(B d2 i2) = step x b1
turn :: Turn -> Bot -> Bot
turn R (B d i) = B (succ d) i
turn L (B d i) = B (pred d) i

forwardN :: Int -> Bot -> Bot
forwardN n (B d (x, y)) = case d of
  North -> B d (x, y - n)
  East -> B d (x + n, y)
  South -> B d (x, y + n)
  West -> B d (x - n, y)

forward :: Bot -> Bot
forward = forwardN 1

day1 :: IO ()
day1 = do
  input <- map parseIns . splitOn ", " <$> readFile "input1.txt"
  putStrLn $ ("day1a: " ++) $ show $ manhattan' $ index $ foldl' (flip step) (B North (0, 0)) input
  -- putStrLn $ ("day1a: " ++) $ show input
  putStrLn $ ("day1b: " ++) $ show $ manhattan' $ snd $ fromJust $ firstRepeat $ map index $ step' (B North (0, 0)) input
