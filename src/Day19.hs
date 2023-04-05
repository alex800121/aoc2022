module Day19 (day19) where

import MyLib
import Data.List.Split
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char

type Robots = [Int]
type RobotCost = [Geodes]
type Geodes = [Int]
type Blueprint = (Int, RobotCost)

blueprintParser :: Parser Blueprint
blueprintParser = do
  string "Blueprint" <* space
  n <- signedInteger
  char ':' <* space
  string "Each ore robot costs" <* space
  oo <- signedInteger <* string "ore." <* space
  string "Each clay robot costs" <* space
  co <- signedInteger <* string "ore." <* space
  string "Each obsidian robot costs" <* space
  bo <- signedInteger <* string "ore and" <* space
  bc <- signedInteger <* string "clay." <* space
  string "Each geode robot costs" <* space
  go <- signedInteger <* string "ore and" <* space
  gb <- signedInteger <* string "obsidian." <* space
  pure ( n
       , [ [oo, 0, 0, 0]
         , [co, 0, 0, 0]
         , [bo, bc, 0, 0]
         , [go, 0, gb, 0]
         ]
       )
  
day19 :: IO ()
day19 = do
  putStrLn $ ("day19a: " ++) $ show ""
  putStrLn $ ("day19b: " ++) $ show ""
