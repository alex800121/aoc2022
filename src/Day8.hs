{-# LANGUAGE TupleSections #-}
module Day8 where

import MyLib ( Parser, signedInteger, mapFirst, drawGraph )
import Text.Megaparsec ( (<|>), parseMaybe )
import Text.Megaparsec.Char ( string, char )
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

data Ins = Rect Int Int | RotateX Int Int | RotateY Int Int deriving (Show, Eq, Ord)

type LCD = Set Index
type LCD' = Map Index Char
type Index = (Int, Int)

readIns :: Int -> Int -> LCD -> Ins -> LCD
readIns _ _ lcd (Rect x y) = lcd <> Set.fromList [(a, b) | a <- [0 .. x - 1], b <- [0 .. y - 1]]
readIns maxX maxY lcd (RotateX x n) = let
  (t, f) = Set.partition ((== x) . fst) lcd
  in f <> Set.map (fmap ((`mod` maxY) . (+ n))) t
readIns maxX maxY lcd (RotateY y n) = let
  (t, f) = Set.partition ((== y) . snd) lcd
  in f <> Set.map (mapFirst ((`mod` maxX) . (+ n))) t

toLCD' :: LCD -> LCD'
toLCD' = Map.fromList . map (,'*') . Set.toList 

printLCD :: LCD -> [String]
printLCD = drawGraph (fromMaybe ' ') . toLCD'

inputParser :: Parser Ins
inputParser = parseRect <|> parseRotateX <|> parseRotateY

-- rect 1x1
parseRect :: Parser Ins
parseRect = Rect <$> (string "rect " >> signedInteger <* char 'x') <*> signedInteger

-- rotate row y=0 by 5
parseRotateY :: Parser Ins
parseRotateY = 
  RotateY <$> (string "rotate row y=" >> signedInteger <* string " by ") <*> signedInteger

parseRotateX :: Parser Ins
parseRotateX =
  RotateX <$> (string "rotate column x=" >> signedInteger <* string " by ") <*> signedInteger

day8 :: IO ()
day8 = do
  ins <- map (fromJust . parseMaybe inputParser) . lines <$> readFile "input8.txt"
  let ans = foldl' (readIns 50 6) Set.empty ins
  putStrLn $ ("day8a: " ++) $ show $ length ans
  putStrLn "day8b: "
  mapM_ putStrLn $ printLCD ans
