module Day18 (day18) where

import Paths_AOC2022
import MyLib
import Data.List.Split
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Data.Bifunctor

type V3 n = Vec (S (S (S Z))) n
type Index = V3 Int

isAdjacent :: Enum a => Vec n a -> Vec n a -> Bool
isAdjacent x y = manhattan (vZipWith (subtract `on` fromEnum) x y) == 1

inBound :: Ord a => (Vec n a, Vec n a) -> Vec n a -> Bool
inBound (minV, maxV) x = and (vZipWith (<=) minV x) && and (vZipWith (>=) maxV x)

adjacents :: Set Index
adjacents = Set.fromList $ map (toVec (SS (SS (SS SZ)))) [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, -1], [0, 0, 1]] 

flood :: (Index -> Bool) -> Set Index -> Set Index -> Set Index
flood f acc starts
  | Set.null starts = acc
  | otherwise = flood f acc' starts'
  where
    acc' = Set.union starts acc
    starts' = (Set.\\ acc) $ Set.filter f $ Set.unions $ Set.map (\x -> Set.map (vZipWith (+) x) adjacents) starts
    
calcSurface :: Index -> Index -> Int
calcSurface (Cons a (Cons b (Cons c Nil))) (Cons d (Cons e (Cons f Nil))) = 2 * sum [x * y, y * z, z * x]
  where
    x = d - a + 1
    y = e - b + 1
    z = f - c + 1

day18 :: IO ()
day18 = do
  input <- map (toVec (SS (SS (SS SZ))) . map (read @Int) . splitOn ",") . lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt")) 
  -- input <- map (toVec (SS (SS (SS SZ))) . map (read @Int) . splitOn ",") . lines <$> readFile "test18.txt"
  let day18a n = (6 * length n) - sum [1 | x <- n, y <- n, isAdjacent x y]
      minMax = bimap (subtract 1) (+ 1) $ foldl' (\(x, y) z -> (vZipWith min x z, vZipWith max y z)) (pure maxBound, pure minBound) input
  putStrLn $ ("day18a: " ++) $ show $ day18a input
  putStrLn $ ("day18b: " ++) $ show $ subtract (uncurry calcSurface minMax) $ day18a $ Set.toList $ flood (\x -> inBound minMax x && x `Set.notMember` Set.fromList input) Set.empty (Set.singleton (fst minMax))
