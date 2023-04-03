module Day2 (day2) where

import MyLib
import Data.Bifunctor

data RPS = Rock | Paper | Scissors deriving (Show, Eq, Ord)
instance Enum RPS where
  toEnum n = case n `mod` 3 of
    0 -> Rock
    1 -> Paper
    2 -> Scissors
  fromEnum Rock = 0
  fromEnum Paper = 1
  fromEnum Scissors = 2

guideA :: String -> RPS
guideA "A" = Rock
guideA "B" = Paper
guideA "C" = Scissors
guideA "X" = Rock
guideA "Y" = Paper
guideA "Z" = Scissors

outcomeScore :: RPS -> RPS -> Int
outcomeScore oppo me
  | oppo == me = 3
  | oppo == succ me = 0
  | oppo == pred me = 6

playerScore :: RPS -> Int
playerScore = (+ 1) . fromEnum

roundScore :: RPS -> RPS -> Int
roundScore oppo me = outcomeScore oppo me + playerScore me

calcPlayer :: (String, String) -> (RPS, RPS)
calcPlayer (oppo, outcome) = (oppo', me oppo')
  where
    oppo' = guideA oppo
    me = case outcome of
      "X" -> pred
      "Y" -> id
      "Z" -> succ
      
day2 :: IO ()
day2 = do
  guide <- map ((\(x : y : _) -> (x, y)) . words) . lines <$> readFile "input2.txt"
  putStrLn $ ("day2a: " ++) $ show $ sum $ map (uncurry roundScore . bimap guideA guideA) guide
  putStrLn $ ("day2b: " ++) $ show $ sum $ map (uncurry roundScore . calcPlayer) guide