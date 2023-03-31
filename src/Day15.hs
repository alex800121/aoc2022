module Day15 (day15) where

import MyLib 
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (space, char, string)
import Data.Maybe (fromJust)
import Debug.Trace
import Data.List

data Disk = D { cyc :: Int, offset :: Int } deriving (Show, Eq, Ord)

diskParser :: Parser Disk
diskParser = do
  d <- string "Disc #" >> signedInteger
  c <- string " has " >> signedInteger
  o <- string " positions; at time=0, it is at position " >> signedInteger <* char '.' <* space
  return $ D c (negate (o + d) `mod` c)

disk0 :: Disk
disk0 = D 1 0

combineDisk :: Disk -> Disk -> Disk
combineDisk (D c1 o1) (D c2 o2) = D (lcm c1 c2) ((o2 - (c2 * y * m)) `mod` c3)
  where
    (n, x, y) = emcd c1 c2
    c3 = lcm c1 c2
    m = (o2 - o1) `div` n


-- c1 a + o1 = c2 b + o2 = t
-- c1 a = c2 b + o2 - o1 = t - o1
-- c1 a - c2 b = o2 - o1 = (t - o1) - c2 b
-- c1 x + c2 y = n
-- m = (o2 - o1) `div` n
-- c1 (x * m) + c2 (y * m) = m * n = o2 - o1 = (t - o1) - c2 b
-- --------------------------------- o2 - o1 = (t - o1) + c2 * (y * m)
-- t = o2 - o1 - (c2 * y * m) + o1 = o1 - c2 * y * m


day15 :: IO ()
day15 = do
  -- disks <- map (fromJust . parseMaybe diskParser) . lines <$> readFile "test15.txt"
  disks <- map (fromJust . parseMaybe diskParser) . lines <$> readFile "input15.txt"
  let disks' = disks ++ [D 11 (negate 7 `mod` 11)]
  putStrLn $ ("day15a: " ++) $ show $ offset $ foldl' combineDisk disk0 disks
  putStrLn $ ("day15b: " ++) $ show $ offset $ foldl' combineDisk disk0 disks'
