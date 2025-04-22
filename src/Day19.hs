module Day19 (day19) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List (foldl')
import Data.Maybe (fromJust)
import MyLib (Parser, signedInteger)
import Paths_AOC2022 (getDataDir)
import Text.Megaparsec (MonadParsec (eof), parseMaybe, (<|>))
import Text.Megaparsec.Char (char, space, string)

type Robots = [Int]

type Blueprint = (Resources, [(Robots, Resources)], Robots)

type Resources = [Int]

data GameState = G {_timer :: Int, _robots :: Robots, _resources :: Resources, _geodes :: Int}
  deriving (Show, Eq, Ord)

dfs :: Blueprint -> GameState -> Int
dfs (geodeCost, bp, maxRobots) = go 0
  where
    calcHue timer g = g + ((timer - 1) * timer) `div` 2
    go best (G 0 _ resources geodes) = max best geodes
    go best g@(G timer robots resources geodes)
      | calcHue timer geodes <= best = best
      | otherwise = foldl' go best (nextGeodes ++ next)
      where
        nextGeodes =
          [ G timer' robots re geodes'
            | tick <- f 0 resources geodeCost robots,
              let re = zipWith3 (\x y z -> x + y * tick - z) resources robots geodeCost,
              let timer' = timer - tick,
              let geodes' = geodes + timer',
              calcHue timer' geodes' >= best
          ]
        timerGeodes = map _timer nextGeodes
        next =
          [ G timer' ro re geodes
            | (added, costs) <- bp,
              let ro = zipWith (+) added robots,
              and (zipWith (<=) ro maxRobots),
              tick <- f 0 resources costs robots,
              let re = zipWith3 (\x y z -> x + y * tick - z) resources robots costs,
              let timer' = timer - tick,
              calcHue timer' geodes >= best,
              all (< timer') timerGeodes
          ]
        f tick [] _ _ = [tick + 1]
        f tick (x : xs) (c : cs) (y : ys)
          | cost <= 0 = f tick xs cs ys
          | y <= 0 = []
          | t + 1 >= timer = [timer]
          | otherwise = f (max tick t) xs cs ys
          where
            cost = c - x
            t = negate ((-cost) `div` y)

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
      let robotCosts = [[0, bc, bo], [0, 0, co], [0, 0, oo]]
          geodeCost = [gb, 0, go]
          maxRobots = maxBound : tail (foldl' (zipWith max) [0, 0, 0] robotCosts)
      ( ( n,
          (geodeCost, zip [[1, 0, 0], [0, 1, 0], [0, 0, 1]] robotCosts, maxRobots)
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
    $ parMap rpar (\(i, bp) -> i * dfs bp (G 24 [0, 0, 1] [0, 0, 0] 0)) input
  putStrLn
    . ("day19b: " ++)
    . show
    . product
    . parMap rpar (\(_, bp) -> dfs bp (G 32 [0, 0, 1] [0, 0, 0] 0))
    $ take 3 input
