module Day22 (day22) where

import MyLib
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Char
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (mapM_)

type Index = (Int, Int)
data Partition = P { size :: Int, used :: Int, avail :: Int }
  deriving (Show, Eq, Ord)
type Disk = Map Index Partition
data GameState = G { end :: Index, start :: Index, disk :: Disk, steps :: Int }
  deriving (Show, Eq)
instance Ord GameState where
  compare (G e1 s1 d1 st1) (G e2 s2 d2 st2) =
       compare (st1 + manhattan' s1 e1) (st2 + manhattan' s2 e2)
    <> compare st1 st2
    <> compare s1 s2
    <> compare e1 e2
    <> compare d1 d2

adjacent :: [Index]
adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

partitionParser :: Parser Disk
partitionParser = do
  string "/dev/grid/node-x"
  x <- signedInteger
  string "-y"
  y <- signedInteger
  some (satisfy (not . isNumber))
  size <- signedInteger
  some (satisfy (not . isNumber))
  used <- signedInteger
  some (satisfy (not . isNumber))
  avail <- signedInteger
  some anySingle
  pure $ Map.singleton (x, y) (P size used avail)

viablePairsN :: [Partition] -> Int
viablePairsN xs = sum $ map f ts
  where
    ts = map (`splitAt` xs) [0 .. (length xs - 1)]
    f (y, ys) = let z = head ys in if used z == 0 then 0 else length $ filter ((used z <=) . avail) (tail ys ++ y)

viable :: Partition -> Partition -> Bool
viable from to = used from <= avail to

manhattan' :: Index -> Index -> Int
manhattan' (x, y) (z, w) = abs (x - z) + abs (y - w)

choices :: GameState -> Set GameState
choices (G e s d st) = undefined
  where
    s' = mapMaybe ((d Map.!?) . (+& s)) adjacent

testDisk :: Disk -> [String]
testDisk = drawGraph f
  where
    f Nothing = '#'
    f (Just p)
      | used p > 100 = 'X'
      | used p >= 80 = '?'
      | used p >= 60 = '.'
      | otherwise = '!'

-- 7 + 14 + 14 + 5 * 33
day22 :: IO ()
day22 = do
  disk <- Map.unions . map (fromJust . parseMaybe partitionParser) . drop 2 . lines <$> readFile "input22.txt"
  putStrLn $ ("day22a: " ++) $ show $ viablePairsN $ Map.elems disk
  putStrLn $ ("day22b: " ++) $ show $ 7 + 14 + 14 + 5 * 33
{-

...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
.....................XXXXXXXXXXXXXX
...................................
...................................
...........................!.......
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................

-}
