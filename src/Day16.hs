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
import Data.List
import Data.Bifunctor
import Debug.Trace
import Data.Either
import Data.Maybe
import Data.Semigroup (Arg(..))

type Valve = String
type ValveFlow = Map Valve Int
type ValveMap = Map Valve (Map Valve Int)
data GameState = G { accPressure :: Int, timer :: Int, atValve :: Valve, openPressure :: Int, openedValves :: Set Valve, closedValves :: ValveFlow }
  deriving (Show, Eq, Ord)

choices :: ValveMap -> GameState -> Set GameState
choices valveMap g@(G accPressure time atValve openPressure openedValves closedValves)
  | Set.null nextValves = Set.singleton $ G (accPressure + time * openPressure) 0 atValve openPressure openedValves closedValves
  | otherwise = Set.unions $ Set.map (choices valveMap) nextValves
  where
    nextValves = Set.filter ((>= 0) . timer) $ Set.map (\(Arg k a) -> f k a) $ Map.argSet closedValves
    f v a = G (accPressure + t * openPressure) (time - t) v (openPressure + a) (Set.insert v openedValves) (Map.delete v closedValves)
      where
        t = (valveMap Map.! atValve) Map.! v

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
  | acc == acc' = Map.map (Map.map (+ 1) . Map.filterWithKey (\k _ -> k == "AA" || k `Map.member` acc)) acc
  | otherwise = buildMap ref acc'
  where
    acc' = Map.mapWithKey f acc
    f k m = Map.delete k . Map.unionWith min m . Map.unionsWith min . Map.mapWithKey g $ m
    g k a = Map.map (+ a) $ ref Map.! k

pickTwo :: GameState -> [GameState] -> Int
pickTwo g gs = maybe 0 ((accPressure g +) . accPressure) (find (Set.disjoint (openedValves g) . openedValves) gs)

day16 :: IO ()
day16 = do
  (valveFlow, valveMap) <- (\(x, y) -> (Map.filterWithKey (\k a -> a /= 0) x, buildMap y (Map.filterWithKey (\k _ -> k == "AA" || (x Map.! k /= 0)) y))) . fromJust . parseMaybe valveParser <$> readFile "input16.txt"
  let initGameState = G 0 30 "AA" 0 Set.empty valveFlow
      initGameState' = G 0 26 "AA" 0 Set.empty valveFlow
      day16a = choices valveMap initGameState
      day16b = map (uncurry pickTwo) $ mapMaybe uncons $ tails $ reverse $ Set.toList $ choices valveMap initGameState'
  putStrLn $ ("day16a: " ++) $ show $ accPressure $ maximum day16a
  putStrLn $ ("day16b: " ++) $ show $ maximum day16b