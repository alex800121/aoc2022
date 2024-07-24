module Day13 (day13) where

import Data.List
import Data.List.Split
import Data.Maybe
import MyLib
import Paths_AOC2022
import Text.Megaparsec
import Text.Megaparsec.Char

data IList a
  = I a
  | L [IList a]
  deriving (Show, Eq)

instance (Ord a) => Ord (IList a) where
  compare (I x) (I y) = compare x y
  compare (L xs) (I y) = compare (L xs) (L [I y])
  compare (I x) (L ys) = compare (L [I x]) (L ys)
  compare (L []) (L []) = EQ
  compare (L []) (L ys) = LT
  compare (L xs) (L []) = GT
  compare (L (x : xs)) (L (y : ys)) = compare x y <> compare xs ys

instance Foldable IList where
  foldMap f (I a) = f a
  foldMap f (L xs) = foldMap (foldMap f) xs

iListParser :: Parser (IList Int)
iListParser = ((I <$> signedInteger) <|> (char '[' >> L <$> many iListParser <* char ']')) <* optional (char ',')

day13 :: IO ()
day13 = do
  input <- map (map (fromJust . parseMaybe iListParser) . lines) . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  let dividers = [L [L [I 2]], L [L [I 6]]]
      input' = sort $ dividers ++ concat input
  putStrLn $ ("day13a: " ++) $ show $ sum $ map (+ 1) $ findIndices (\(x : y : _) -> x <= y) input
  putStrLn $ ("day13b: " ++) $ show $ product $ map (+ 1) $ findIndices (`elem` dividers) input'
