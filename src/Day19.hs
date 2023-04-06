module Day19 (day19) where

import MyLib
import Data.List.Split
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Data.Either
import Debug.Trace

type Robots = [Int]
type RobotCosts = [(Robots, Geodes)]
type Geodes = [Int]
data GameState = G { getTimer :: Int, getGeodes :: Geodes, getRobots :: Robots, getBlueprint :: Int, getRobotCosts :: RobotCosts }
  deriving (Show, Eq, Ord)

blueprintParser :: Parser [GameState]
blueprintParser = (eof >> pure []) <|> do
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
  ( G 24
      [0, 0, 0, 0] [0, 0, 0, 1]
      n
      ( reverse (zip (map reverse [ [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1] ])
                     (map reverse [ [oo, 0, 0, 0], [co, 0, 0, 0] , [bo, bc, 0, 0] , [go, 0, gb, 0] ])
                )
      )
    : ) <$> blueprintParser

choices :: Int -> GameState -> [Either GameState GameState]
choices maxOb (G ti ge ro bl rc)
  | and (zipWith (>=) ge obCost) = pure $ (if ti - 1 == 0 then Left else Right) (G (ti - 1) (zipWith subtract obCost $ zipWith (+) ge ro) ((head ro + 1) : tail ro) bl rc)
  | potential <= maxOb = []
  | otherwise = do
  (addRobots, robotCosts) <- rc
  let needRobots = zipWith subtract ge robotCosts
  t <- (+ 1) . maximum <$> zipWithM myDiv needRobots ro
  case compare t ti of
    LT -> pure $ Right (G (ti - t) (zipWith subtract robotCosts $ zipWith (+) ge (map (* t) ro)) (zipWith (+) ro addRobots) bl rc)
    _  -> pure $ Left (G 0 (zipWith (+) ge (map (* ti) ro)) ro bl rc)
  where
    obCost = snd $ head rc
    potential = head ge + ((ti * ((2 * last ro) + ti - 1)) `div` 2)

day19a :: Int -> [Either GameState GameState] -> Int
day19a m [] = m
day19a m xs = day19a m' (concatMap (choices m') xs')
  where
    (ys, xs') = partitionEithers xs
    m' = maximum (m : map (head . getGeodes) ys)

myDiv :: (Integral a, Alternative m) => a -> a -> m a
myDiv n m
  | n <= 0 = pure 0
  | m == 0 = empty
  | n `mod` m == 0 = pure $ n `div` m
  | otherwise = pure $ 1 + (n `div` m)


day19 :: IO ()
day19 = do
  input <- fromJust . parseMaybe blueprintParser <$> readFile "test19.txt"
  -- input <- fromJust . parseMaybe blueprintParser <$> readFile "input19.txt"
  print input
  putStrLn $ ("day19a: " ++) $ show $ map (day19a 0 . (: []) . Right) input
  putStrLn $ ("day19b: " ++) $ show ""
