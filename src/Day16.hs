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

day16 :: IO ()
day16 = do
  (valveFlow, valveMap) <- fromJust . parseMaybe valveParser <$> readFile "input16.txt"
  putStrLn $ ("day16a: " ++) $ show (valveFlow, valveMap)
  putStrLn $ ("day16b: " ++) $ show ""