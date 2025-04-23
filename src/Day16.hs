{-# LANGUAGE TupleSections #-}

module Day16 (day16) where

import Data.Array.Unboxed qualified as A
import Data.Bifunctor
import Data.Bits
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (foldl', sortBy)
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Vector.Unboxed qualified as V
import Data.Word
import MyLib
import Paths_AOC2022
import Text.Megaparsec
import Text.Megaparsec.Char

type Valve = String

type RawFlow = Map Valve Int

type RawMap = Map Valve (Map Valve Int)

type ValveFlow = V.Vector Int

type ValveMap = A.Array (Int, Int) Int

-- >>> product (replicate (64 - 25 + 1) 2)
-- 1099511627776

-- 0..15 open valves (bits)
-- 16..19 current valves (<= 16)
-- 20..24 timer (<= 30)
-- 25.. total pressure (<= 6480)
type Compress = Int

type GameState = (Int, Int, Int)

solveB :: [(Int, Int)] -> Int
solveB xs = go 0 xs'
  where
    xs' = sortBy (flip compare `on` snd) (map (first (`mod` bit 16)) xs)
    go n (x : y : xs)
      | snd y + snd x <= n = n
      | otherwise = go' n x (y : xs)
      where
        go' n _ [] = go n (y : xs)
        go' n a (b : bs)
          | s <= n = go n (y : xs)
          | fst a .&. fst b == 0 = go s (y : xs)
          | otherwise = go' n a bs
          where
            s = snd a + snd b
    go n _ = n

dfs :: Bool -> ValveFlow -> ValveMap -> GameState -> (Int, IntMap Int)
dfs b vf vm = go (0, IM.empty) . (,0)
  where
    ideal timer start visited n =
      n
        + sum
          [ timer' * (vf V.! x)
            | x <- [0 .. 15],
              not (testBit visited x),
              let t = vm A.! (start, x),
              t < timer,
              let timer' = timer - vm A.! (start, x)
          ]
    go (best, cache) (g@(timer, start, visited), n)
      | Just n0 <- cache IM.!? g',
        n0 >= n =
          (best, cache)
      | otherwise = foldl' go (best', cache') next
      where
        best' = max best n
        g' = compress g
        cache' = IM.insert g' n cache
        next =
          [ ((timer', start', visited'), n')
            | start' <- [0 .. 15],
              not (testBit visited start'),
              let t = vm A.! (start, start'),
              t < timer,
              let timer' = timer - t,
              let visited' = setBit visited start',
              let n' = n + timer' * vf V.! start',
              b || ideal timer' start' visited' n' >= best'
          ]

valveParser :: Parser (RawFlow, RawMap)
valveParser =
  (eof >> pure (Map.empty, Map.empty)) <|> do
    name <- string "Valve " >> some (anySingleBut ' ') <* space
    n <- string "has flow rate=" >> signedInteger <* string "; tunnel"
    optional (char 's')
    string " lead"
    optional (char 's')
    string " to valve"
    optional (char 's') <* space
    valves <- splitOn ", " <$> some (anySingleBut '\n') <* space
    bimap (Map.insert name n) (Map.insert name (Map.fromList $ map (,1) valves)) <$> valveParser

buildMap :: RawMap -> RawMap -> (Map String Int, A.Array (Int, Int) Int)
buildMap ref acc
  | acc == acc' = (vti, im)
  | otherwise = buildMap ref acc'
  where
    acc' = Map.mapWithKey f acc
    vm = Map.toList $ Map.map (Map.toList . Map.map (+ 1) . Map.filterWithKey (\k _ -> k == "AA" || k `Map.member` acc)) acc
    vti = Map.fromList (zip (map fst vm) [0 ..])
    im =
      A.accumArray
        (const id)
        0
        ((0, 0), (length vti - 1, length vti - 1))
        [ ((vti Map.! x, vti Map.! y), a)
          | (x, ys) <- vm,
            (y, a) <- ys
        ]
    f k m = Map.delete k . Map.unionWith min m . Map.unionsWith min . Map.mapWithKey g $ m
    g k a = Map.map (+ a) $ ref Map.! k

compress :: GameState -> Compress
compress (timer, start, visited) = timer `shift` 20 + start `shift` 16 + setBit visited start

decompress :: Compress -> GameState
decompress n = (timer, curValve, visited)
  where
    timer = n `shiftR` 20
    curValve = (n - timer `shift` 20) `shiftR` 16
    visited = n `mod` bit 16

day16 :: IO ()
day16 = do
  (vf, (vti, valveMap)) <- (\(x, y) -> (Map.toList $ Map.filterWithKey (\k a -> a /= 0) x, buildMap y (Map.filterWithKey (\k _ -> k == "AA" || (x Map.! k /= 0)) y))) . fromJust . parseMaybe valveParser <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  let valveFlow = V.replicate (length vti) 0 V.// map (first (vti Map.!)) vf
  putStrLn
    . ("day16a: " ++)
    . show
    . fst
    $ dfs False valveFlow valveMap (30, 0, 0)
  putStrLn
    . ("day16b: " ++)
    . show
    . solveB
    . IM.toList
    . snd
    $ dfs True valveFlow valveMap (26, 0, 0)
