{-# LANGUAGE TupleSections #-}
module Day16 (day16) where

import MyLib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List.Split
import Data.Bifunctor
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace

type Valve = String
type ValveFlow = Map Valve Int
type ValveMap = Map Valve (Map Valve Int)
data GameState = G { accPressure :: Int, timer :: Int, atValve :: Valve, openPressure :: Int, closedValves :: ValveFlow, valveMap :: ValveMap }
  deriving (Show, Eq, Ord)

choices :: GameState -> Either GameState (Set GameState)
choices g@(G accPressure timer atValve openPressure closedValves valveMap)
  | timer <= 0 = Left g
  | Set.null nextValves = Left $ G (accPressure + timer * openPressure) 0 atValve openPressure closedValves valveMap
  | otherwise = undefined
  where
    nextValves = undefined
    f v = let
      t = undefined
      in undefined

timeLimitA :: Int
timeLimitA = 30

valveParser :: Parser (ValveFlow, ValveMap)
valveParser = (eof >> pure (Map.empty, Map.empty)) <|> do
  name <- string "Valve " >> some (anySingleBut ' ') <* space
  n <- string "has flow rate=" >> signedInteger <* string "; tunnel"
  optional (char 's')
  string " lead" 
  optional (char 's')
  string " to valve"
  optional (char 's') <* space
  valves <- splitOn ", " <$> some (anySingleBut '\n') <* space
  bimap (Map.insert name n) (Map.insert name (Map.fromList $ map (, 1) valves)) <$> valveParser

buildMap :: ValveMap -> ValveMap -> ValveMap
buildMap ref acc
  -- | trace (show acc) False = undefined
  | acc == acc' = Map.map (Map.filterWithKey (\k _ -> k == "AA" || k `Map.member` acc)) acc
  | otherwise = buildMap ref acc'
  where
    acc' = Map.mapWithKey f acc
    f k m = Map.delete k . Map.unionWith min m . Map.unionsWith min . Map.mapWithKey g $ m
    g k a = Map.map (+ a) $ ref Map.! k

day16 :: IO ()
day16 = do
  -- (valveFlow, valveMap) <- (\(x, y) -> (Map.filterWithKey (\k a -> a /= 0 || k == "AA") x, buildMap y (Map.filterWithKey (\k _ -> k == "AA" || (x Map.! k /= 0)) y))) . fromJust . parseMaybe valveParser <$> readFile "input16.txt"
  (valveFlow, valveMap) <- (\(x, y) -> (Map.filter (/= 0) x, buildMap y (Map.filterWithKey (\k _ -> k == "AA" || (x Map.! k /= 0)) y))) . fromJust . parseMaybe valveParser <$> readFile "test16.txt"
  putStrLn $ ("day16a: " ++) $ show ""
  putStrLn $ ("day16b: " ++) $ show ""