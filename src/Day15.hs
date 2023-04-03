module Day15 (day15) where

import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Data.List (nub, sort)

type Index = (Int, Int)
type Range = (Int, Int)
type Sensor = Vec (S (S Z)) Range
type Sensor' = (Index, Index)

manhattan' :: Index -> Index -> Int
manhattan' (a, b) (c, d) = abs (a - c) + abs (b - d)

withinRange :: Index -> Sensor -> Bool
withinRange (x, y) (Cons (a, b) (Cons (c, d) Nil)) =
     x + y >= a
  && x + y < b
  && x - y >= c
  && x - y < d
  
sensorParser :: Parser Sensor
sensorParser = do
  string "Sensor at x="
  x <- signedInteger
  string ", y="
  y <- signedInteger
  string ": closest beacon is at x="
  x' <- signedInteger
  string ", y="
  y' <- signedInteger
  let a = (x, y)
      r = abs (x - x') + abs (y - y')
  pure $ Cons (x + y - r, x + y + r + 1) $ Cons (x - y - r, x - y + r + 1) Nil
  --          x + y + a ~ x + y + b     x - y + a ~ x - y + b

atY :: Int -> Sensor -> Maybe (Int, Int)
atY y s@(Cons (a, b) (Cons (c, d) Nil))
  | withinRange (x1, y) s = Just (x1, x2)
  | withinRange (x3, y) s = Just (x3, x4)
  | otherwise = Nothing
  where
    x1 = c + y
    x2 = b - y
    x3 = a - y
    x4 = d + y
    
sensorParser' :: Parser Sensor'
sensorParser' = do
  string "Sensor at x="
  x <- signedInteger
  string ", y="
  y <- signedInteger
  string ": closest beacon is at x="
  x' <- signedInteger
  string ", y="
  y' <- signedInteger
  pure ((x, y), (x', y'))

fuseRange :: Ord a => [(a, a)] -> [(a, a)]
fuseRange [] = []
fuseRange [x] = [x]
fuseRange (x : y : xs) = if snd x >= fst y then fuseRange ((fst x, max (snd x) (snd y)) : xs) else x : fuseRange (y : xs)

contains :: Sensor -> Sensor -> Bool
contains s1 s2 = Just s2 == overlapEucVec s1 s2

day15 :: IO ()
day15 = do
  -- sensors <- map (fromJust . parseMaybe sensorParser) . lines <$> readFile "test15.txt"
  -- sensors' <- map (fromJust . parseMaybe sensorParser') . lines <$> readFile "test15.txt"
  sensors <- map (fromJust . parseMaybe sensorParser) . lines <$> readFile "input15.txt"
  sensors' <- map (fromJust . parseMaybe sensorParser') . lines <$> readFile "input15.txt"
  let xs = concatMap (\(a, b) -> [fst a - manhattan' a b, fst a + manhattan' a b]) sensors'
      minX = minimum xs
      maxX= maximum xs
      y = 2000000
      y' = 10
      beaconsAtY n = nub $ filter ((== n) . snd) $ map snd sensors'
      searchArea
  putStrLn $ ("day15a: " ++) $ show $ subtract (length (beaconsAtY y)) $ sum $ map (uncurry subtract) $ fuseRange $ sort $ mapMaybe (atY y) sensors
  putStrLn $ ("day15b: " ++) $ show $ subtractEucVecs sensors []