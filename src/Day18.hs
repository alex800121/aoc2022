module Day18 (day18) where

import Control.Monad (foldM)
import Control.Monad.ST.Lazy qualified as Lazy
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.List.Split
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M
import Linear.V3
import Paths_AOC2022

type Index = Int

type Cubes = V.Vector Bool

type STCubes s = M.STVector s Bool

factor = 23

toV3 i = V3 (i `div` (factor * factor)) ((i `div` factor) `mod` factor) (i `mod` factor)

fromV3 (V3 a b c) = (a * factor * factor) + (b * factor) + c

inBound :: (Int, Int) -> Int -> Bool
inBound (minI, maxI) i = and (mzipWith (<=) minV x) && and (mzipWith (>=) maxV x)
  where
    [minV, maxV, x] = map toV3 [minI, maxI, i]

adjacents = map fromV3 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 (-1), V3 0 0 1]

calcSurface :: Index -> Index -> Int
calcSurface i0 i1 = 2 * sum [x * y, y * z, z * x]
  where
    (V3 a b c) = toV3 i0
    (V3 d e f) = toV3 i1
    x = d - a + 1
    y = e - b + 1
    z = f - c + 1

flood :: ((Int, Int), Cubes) -> Cubes
flood (b@(start, _), cubes) = V.create $ do
  acc <- M.replicate (V.length cubes) False
  M.write acc start True
  let go [] = pure acc
      go xs = foldM f [] next >>= go
        where
          next = concatMap (\x -> map (+ x) adjacents) xs
      f accxs i | inBound b i && not (cubes V.! i) = do
        check <- M.read acc i
        if check then pure accxs else M.write acc i True >> pure (i : accxs)
      f accxs i = pure accxs
  go [start]

day18a v = V.sum $ V.imap (\i x -> if x then length [() | y <- adjacents, Just True /= (v V.!? (i + y))] else 0) v

readInput :: [V3 Int] -> ((Int, Int), Cubes)
readInput xs = Lazy.runST $ Lazy.fixST go
  where
    go :: ((Int, Int), Cubes) -> Lazy.ST s ((Int, Int), Cubes)
    go output = do
      v <- M.replicate (snd (fst output) + 1) False
      let g (lb, hb) [] = pure (fromV3 (lb - 1), fromV3 (hb + 1))
          g (lb, hb) (x : xs) = M.write v x' True >> g (mzipWith min lb x, mzipWith max hb x) xs
            where
              x' = fromV3 x
      (,) <$> g (pure maxBound, pure minBound) xs <*> V.freeze v

day18 :: IO ()
day18 = do
  input0 <- map ((\[x, y, z] -> V3 x y z) . map (succ . read @Int) . splitOn ",") . lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  let (minMax, input) = readInput input0
  putStrLn
    . ("day18a: " ++)
    . show
    $ day18a input
  putStrLn
    . ("day18b: " ++)
    . show
    . subtract (uncurry calcSurface minMax)
    . day18a
    $ flood (minMax, input)
