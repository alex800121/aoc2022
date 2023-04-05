module Day18 (day18) where

import MyLib
import Data.List.Split
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

type V3 n = Vec (S (S (S Z))) n

isAdjacent :: Enum a => Vec n a -> Vec n a -> Bool
isAdjacent x y = manhattan (vZipWith (subtract `on` fromEnum) x y) == 1

inBound :: Ord a => (Vec n a, Vec n a) -> Vec n a -> Bool
inBound (minV, maxV) x = and (vZipWith (<=) minV x) && and (vZipWith (>=) maxV x)

day18 :: IO ()
day18 = do
  input <- map (toVec (SS (SS (SS SZ))) . map (read @Int) . splitOn ",") . lines <$> readFile "input18.txt"
  -- input <- map (toVec (SS (SS (SS SZ))) . map (read @Int) . splitOn ",") . lines <$> readFile "test18.txt"
  let day18a = (6 * length input) - sum [1 | x <- input, y <- input, isAdjacent x y]
      minMax = foldl' (\(x, y) z -> (vZipWith min x z, vZipWith max y z)) (pure maxBound, pure minBound) input
  putStrLn $ ("day18a: " ++) $ show day18a
  putStrLn $ ("day18b: " ++) $ show minMax