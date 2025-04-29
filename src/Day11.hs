module Day11 (day11) where

import Data.Char
import Data.Foldable (foldl')
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Debug.Trace
import MyLib
import Paths_AOC2022
import Text.Megaparsec
import Text.Megaparsec.Char

data Monkey = M {items :: [Int], operation :: Int -> Int, test :: Int, ifTrue :: Int, ifFalse :: Int, inspected :: Int}

type Monkeys = Vector Monkey

instance Show Monkey where
  show x = show (items x, ifTrue x, ifFalse x)

monkeyInspects :: (Int -> Int) -> Monkeys -> Int -> Monkeys
monkeyInspects x ms k = ms'
  where
    m = ms V.! k
    m' = m {items = [], inspected = length (items m) + inspected m}
    ms' = foldl' f (V.modify (\v -> MV.write v k m') ms) (x . operation m <$> items m)
    f acc n
      | n `mod` test m == 0 = V.modify (\v -> MV.modify v (\x -> x {items = n : items x}) (ifTrue m)) acc
      | otherwise = V.modify (\v -> MV.modify v (\x -> x {items = n : items x}) (ifFalse m)) acc

monkeysParser :: Parser [(Int, Monkey)]
monkeysParser =
  (eof >> pure []) <|> do
    n <- string "Monkey " >> signedInteger <* char ':' <* space
    items <- map (read @Int) . splitOn ", " <$> (string "Starting items: " >> some (anySingleBut '\n') <* space)
    operation <- string "Operation: new = " >> opParser
    test <- string "Test: divisible by " >> signedInteger <* space
    ifTrue <- string "If true: throw to monkey " >> signedInteger <* space
    ifFalse <- string "If false: throw to monkey " >> signedInteger <* space
    ((n, M items operation test ifTrue ifFalse 0) :) <$> monkeysParser

opParser :: Parser (Int -> Int)
opParser = do
  a <- const <$> signedInteger <|> (some (anySingleBut ' ') >> pure id)
  space
  x <- (char '+' >> pure (+)) <|> (char '*' >> pure (*))
  space
  b <- const <$> signedInteger <|> (some (satisfy (not . isSpace)) >> pure id)
  space
  pure $ \y -> x (a y) (b y)

day11 :: IO ()
day11 = do
  -- monkeys <- (\x -> V.replicate (length x) undefined V.// x) . fromJust . parseMaybe monkeysParser <$> readFile "test11.txt"
  monkeys <-
    (\x -> V.replicate (length x) undefined V.// x)
      . fromJust
      . parseMaybe monkeysParser
      <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  let cm = V.foldl1' lcm $ V.map test monkeys
  putStrLn
    . ("day11a: " ++)
    . show
    . product
    . take 2
    . sortBy (comparing Down)
    . map inspected
    . V.toList
    . (!! 20)
    $ iterate (\x -> foldl' (monkeyInspects (`div` 3)) x [0 .. (length x - 1)]) monkeys
  putStrLn
    . ("day11b: " ++)
    . show
    . product
    . take 2
    $ sortBy
      (comparing Down)
      ( map inspected
          . V.toList
          . (!! 10000)
          $ iterate (\x -> foldl' (monkeyInspects (`mod` cm)) x [0 .. (length x - 1)]) monkeys
      )
