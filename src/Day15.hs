module Day15 (day15) where

import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Data.List (nub, sort, foldl', findIndex, find)

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

toSensor :: Sensor' -> Sensor
toSensor (a@(x, y), b@(x', y')) = let r = manhattan' a b in Cons (x + y - r, x + y + r + 1) $ Cons (x - y - r, x - y + r + 1) Nil
    
toCenter :: Sensor -> Index
toCenter (Cons (a, b) (Cons (c, d) Nil)) = let
  x = (a + b - 1) `div` 2
  y = (c + d - 1) `div` 2
  x' = (x + y) `div` 2
  y' = (x - y) `div` 2
  in (x', y')

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
      searchArea = [toSensor ((2000000, 2000000), (0, 4000000))]
      -- searchArea' = [toSensor ((11, 14), (11, 23))]
      findY = fromJust $ findIndex (\x -> length x /= 1 && (let x' = snd $ head x in x' <= 4000000 && x' >= 0)) $ map (\x -> fuseRange . sort $ mapMaybe (atY x) sensors) [0..4000000]
  putStrLn $ ("day15a: " ++) $ show $ subtract (length (beaconsAtY y)) $ sum $ map (uncurry subtract) $ fuseRange $ sort $ mapMaybe (atY y) sensors
  -- putStrLn $ ("day15b: " ++) $ show $ 4000000 * fromIntegral (snd $ head $ fuseRange $ sort $ mapMaybe (atY findY) sensors) + findY
  putStrLn $ ("day15b: " ++) $ show $ (\(a, b) -> 4000000 * toInteger a + toInteger b) $ fromJust $ find (\(x, y) -> x >= 0 && y >= 0 && x <= 4000000 && y <= 4000000) $ map toCenter $ subtractEucVecs sensors searchArea
  -- print sensors