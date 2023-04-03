module Day11 (day11) where

import MyLib
import Data.List
import Data.List.Split
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Maybe (fromJust)
import Data.Char
import Debug.Trace

data Monkey = M { items :: [Integer], operation :: Integer -> Integer, test :: Integer, ifTrue :: Integer, ifFalse :: Integer, inspected :: Integer }
type Monkeys = Vector Monkey

instance Show Monkey where
  show x = show (items x, ifTrue x, ifFalse x)

monkeyInspects :: (Integer -> Integer) -> Monkeys -> Integer -> Monkeys
-- monkeyInspects x ms k | trace (show ms) False = undefined
monkeyInspects x ms k = ms'
  where
    m = ms V.! fromIntegral k
    (t, f) = partition ((== 0) . (`mod` test m)) $ map (x . operation m) $ items m
    m' = m { items = [], inspected = fromIntegral (length (items m)) + inspected m}
    ms' = V.modify (\v -> MV.write v (fromIntegral k) m')
        $ V.modify (\v -> MV.modify v (\n -> n { items = t ++ items n }) (fromIntegral $ ifTrue m))
        $ V.modify (\v -> MV.modify v (\n -> n { items = f ++ items n }) (fromIntegral $ ifFalse m)) ms

monkeysParser :: Parser [(Int, Monkey)]
monkeysParser = (eof >> pure []) <|> do
  n <- string "Monkey " >> signedInteger <* char ':' <* space
  items <- map (read @Integer) . splitOn ", " <$> (string "Starting items: " >> some (anySingleBut '\n') <* space)
  operation <- string "Operation: new = " >> opParser
  test <- string "Test: divisible by " >> fromIntegral <$> signedInteger <* space
  ifTrue <- string "If true: throw to monkey " >> fromIntegral <$> signedInteger <* space
  ifFalse <- string "If false: throw to monkey " >> fromIntegral <$> signedInteger <* space
  ((n, M items operation test ifTrue ifFalse 0) :) <$> monkeysParser

opParser :: Parser (Integer -> Integer)
opParser = do
  a <- (const . fromIntegral <$> signedInteger) <|> (some (anySingleBut ' ') >> pure id)
  space
  x <- (char '+' >> pure (+)) <|> (char '*' >> pure (*))
  space
  b <- (const . fromIntegral <$> signedInteger) <|> (some (satisfy (not . isSpace)) >> pure id)
  space
  pure $ \y -> x (a y) (b y)
  
day11 :: IO ()
day11 = do
  -- monkeys <- (\x -> V.replicate (length x) undefined V.// x) . fromJust . parseMaybe monkeysParser <$> readFile "test11.txt"
  monkeys <- (\x -> V.replicate (length x) undefined V.// x) . fromJust . parseMaybe monkeysParser <$> readFile "input11.txt"
  let test' = V.foldl1' lcm $ V.map test monkeys
  print test'
  putStrLn $ ("day11a: " ++) $ show $ product $ take 2 $ reverse $ sort $ map inspected $ V.toList $ (!! 20) $ iterate (\x -> foldl' (monkeyInspects (`div` 3)) x [0 .. (fromIntegral (length x) - 1)]) monkeys
  putStrLn $ ("day11b: " ++) $ show $ product $ take 2 $ reverse $ sort $ map inspected $ V.toList $ (!! 10000) $ iterate (\x -> foldl' (monkeyInspects (`mod` test')) x [0 .. (fromIntegral (length x) - 1)]) monkeys