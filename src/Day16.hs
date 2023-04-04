{-# LANGUAGE TupleSections #-}
module Day16 (day16) where

import MyLib
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List.Split
import Data.Bifunctor
import Data.Maybe (fromJust)
import Debug.Trace

type Valve = String
type ValveFlow = Map Valve Int
type ValveMap = Map Valve (Map Valve Int)

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
  (valveFlow, valveMap) <- (\(x, y) -> (Map.filterWithKey (\k a -> a /= 0 || k == "AA") x, buildMap y (Map.filterWithKey (\k _ -> k == "AA" || (x Map.! k /= 0)) y))) . fromJust . parseMaybe valveParser <$> readFile "input16.txt"
  print valveFlow
  print valveMap
  putStrLn $ ("day16a: " ++) $ show ""
  putStrLn $ ("day16b: " ++) $ show ""