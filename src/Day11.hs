{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Day11 (day11) where

import MyLib
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Text.Megaparsec
import Text.Megaparsec.Char 
import Data.List.Split
import Data.Maybe (fromJust)
import Data.List
import Data.Either
import Control.Monad (guard)
import Debug.Trace (trace)

type Floor = Set (Either String String)
-- type Floor = [Either String String]
-- data Floor = F { microchips :: Set Microchips, generators :: Set Generators } 
-- data GameState = G { elevator :: Int, floors :: Vector Floor, steps :: Int }
data GameState = G { elevator :: Int, floors :: Vector Floor }
  deriving (Show, Eq, Ord)
  -- deriving (Show, Eq)
instance {-# OVERLAPPING #-} Ord (GameState, Int) where
  compare (G e1 f1, n1) (G e2 f2, n2) = compare (f f1 + n1) (f f2 + n2) <> compare f1 f2 <> compare e1 e2
    where
      f = Vector.ifoldl' (\acc i b -> (length b * (3 - i)) + acc) 0

inputParser :: Parser (Vector Floor)
inputParser = do
  string "The "
  n <-
    ( (string "first" >> pure 0) <|>
      (string "second" >> pure 1) <|>
      (string "third" >> pure 2) <|>
      (string "fourth" >> pure 3)
    ) <* space <* string "floor contains " 
  floors <- (nothingParser <|> gmParser) <* space
  -- next <- (eof >> pure (Vector.replicate 4 [])) <|> inputParser
  next <- (eof >> pure (Vector.replicate 4 Set.empty)) <|> inputParser
  pure $ next Vector.// [(n, floors)]

nothingParser :: Parser Floor
nothingParser = string "nothing relevant." >> pure Set.empty
-- nothingParser = string "nothing relevant." >> pure [] 

gmParser :: Parser Floor
gmParser = do
  string "a " <|> string "and a "
  name <- head . splitOn "-" <$> some (anySingleBut ' ') <* space
  gm <- some (satisfy (`notElem` ",. "))
  sep <- anySingle <* space
  next <- case sep of
    '.' -> pure Set.empty
    -- '.' -> pure []
    _ -> gmParser
  case gm of
    "generator" -> pure $ Set.insert (Right name) next 
    "microchip" -> pure $ Set.insert (Left name) next 
    -- "generator" -> pure $ insert (Right name) next 
    -- "microchip" -> pure $ insert (Left name) next 
    _ -> error "gm"

compatibleFloor :: Floor -> Bool
compatibleFloor f = let (microchips, generators) = partitionEithers $ Set.toList f in
-- compatibleFloor f = let (microchips, generators) = partitionEithers f in
  null (microchips \\ generators) ||
  null (generators \\ microchips)

choices :: GameState -> Set GameState
choices (G elevator floors) = Set.fromList $ do
  nextEle <- filter ((&&) <$> (<= 3) <*> (>= 0)) $ map (+ elevator) [-1, 1]
  let floor = floors Vector.! elevator
      nextFloor = floors Vector.! nextEle
  -- pick2 <- filter ((`elem` [1, 2]) . length) . subsequences $ floor
  pick2 <- filter ((`elem` [1, 2]) . length) . Set.toList. Set.powerSet $ floor
  -- let floor' = floor \\ pick2
  let floor' = floor Set.\\ pick2
      nextFloor' = nextFloor <> pick2
  guard $ compatibleFloor floor' && compatibleFloor nextFloor'
  let floors' = floors Vector.// [(elevator, floor'), (nextEle, nextFloor')]
  pure $ G nextEle floors'

finished :: GameState -> Bool
finished (G _ floors) = all (null . (floors Vector.!)) [0, 1, 2]

step :: Set GameState -> Set (GameState, Int) -> Int
-- step travelled s = if any finished s then 0 else 1 + step (Set.union s travelled) (Set.unions (Set.map choices s) Set.\\ travelled)
-- step travelled s = trace (show (Set.size s)) $ if any finished s then 0 else 1 + step (Set.union s travelled) (Set.unions (Set.map choices s) Set.\\ travelled)
step travelled s = if finished picked then n else step travelled' s'
  where
    ((picked, n), rest) = Set.deleteFindMin s
    travelled' = Set.insert picked travelled
    s' = Set.map (, n + 1) $ (Set.\\ travelled) $ choices picked

day11 :: IO ()
day11 = do
  input <- fromJust . parseMaybe inputParser <$> readFile "input11.txt"
  -- input <- fromJust . parseMaybe inputParser <$> readFile "test11.txt"
  let initGameState = G 0 input
      initGameState' =
        G 0
          (Vector.modify
            ( \x -> MVector.modify x
              (Set.union (Set.fromList [x y | x <- [Left, Right], y <- ["elerium", "dilithium"]])) 0
            ) input)
  putStrLn $ ("day11a: " ++) $ show $ step Set.empty $ Set.singleton (initGameState, 0)
  putStrLn $ ("day11b: " ++) $ show $ step Set.empty $ Set.singleton (initGameState', 0)
