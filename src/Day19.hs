module Day19 (day19) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.Bifunctor (Bifunctor (..))
import Data.Either
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as M
import Debug.Trace
import MyLib
import Paths_AOC2022
import Text.Megaparsec
import Text.Megaparsec.Char

type Robots = [Int]

type Blueprint = ([(Robots, Geodes)], Robots)

type Geodes = [Int]

dfs :: Blueprint -> Int -> (Int, Robots, Geodes) -> Int
dfs (bp, maxRobots) = go
  where
    go best (0, _, resources) = max best (head resources)
    go best (timer, robots, resources)
      | ideal <= best = best
      | otherwise = foldl' go best next
      where
        ideal = head resources + head robots * timer + sum [1 .. timer - 1]
        next =
          [ (timer', ro, re)
            | (added, costs) <- bp,
              let needed = zipWith subtract resources costs,
              let ro = zipWith (+) added robots,
              and (zipWith (<=) ro maxRobots),
              tick <- f 0 needed robots,
              let re = zipWith3 (\x y z -> x + y * tick - z) resources robots costs,
              let timer' = timer - tick
          ]
        f tick [] _ = [tick + 1]
        f tick (x : xs) (y : ys)
          | x <= 0 = f tick xs ys
          | y <= 0 = []
          | t + 1 >= timer = [timer]
          | otherwise = f (max tick t) xs ys
          where
            t = negate ((-x) `div` y)

blueprintParser :: Parser [(Int, Blueprint)]
blueprintParser =
  (eof >> pure [])
    <|> do
      string "Blueprint" <* space
      n <- signedInteger
      char ':' <* space
      string "Each ore robot costs" <* space
      oo <- signedInteger <* space <* string "ore." <* space
      string "Each clay robot costs" <* space
      co <- signedInteger <* space <* string "ore." <* space
      string "Each obsidian robot costs" <* space
      bo <- signedInteger <* space <* string "ore and" <* space
      bc <- signedInteger <* space <* string "clay." <* space
      string "Each geode robot costs" <* space
      go <- signedInteger <* space <* string "ore and" <* space
      gb <- signedInteger <* space <* string "obsidian." <* space
      let robotCosts = [[0, gb, 0, go], [0, 0, bc, bo], [0, 0, 0, co], [0, 0, 0, oo]]
          maxRobots = maxBound : tail (foldl' (zipWith max) [0, 0, 0, 0] robotCosts)
      ( ( n,
          (zip [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]] robotCosts, maxRobots)
        )
          :
        )
        <$> blueprintParser

day19 = do
  input <- fromJust . parseMaybe blueprintParser <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  putStrLn
    . ("day19a: " ++)
    . show
    . sum
    $ map (\(i, bp) -> i * dfs bp 0 (24, [0, 0, 0, 1], [0, 0, 0, 0])) input
  putStrLn
    . ("day19b: " ++)
    . show
    . product
    . map (\(_, bp) -> dfs bp 0 (32, [0, 0, 0, 1], [0, 0, 0, 0]))
    $ take 3 input
