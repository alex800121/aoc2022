module Day9 (day9) where

import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Data.List
import Data.Maybe (fromJust)
import Data.Char (isSpace)

inputParser :: Parser String
inputParser = concat <$> many (decompressParser <|> stringParser)

stringParser :: Parser String
stringParser = some (anySingleBut '(')

decompressParser :: Parser String
decompressParser = do
  char '('
  a <- signedInteger
  char 'x'
  b <- signedInteger
  char ')'
  st <- count a anySingle
  return (concat $ replicate b st)

inputParser' :: Parser Int
inputParser' = sum <$> many (decompressParser' <|> stringParser')

stringParser' :: Parser Int
stringParser' = length <$> some (anySingleBut '(')

decompressParser' :: Parser Int
decompressParser' = do
  char '('
  a <- signedInteger
  char 'x'
  b <- signedInteger
  char ')'
  st <- fromJust . parseMaybe inputParser' <$> count a anySingle
  return (st * b)


day9 :: IO ()
day9 = do
  input <- filter (not . isSpace) <$> readFile "input9.txt"
  putStrLn $ ("day9a: " ++) $ show $ length $ fromJust $ parseMaybe inputParser input
  putStrLn $ ("day9b: " ++) $ show $ fromJust $ parseMaybe inputParser' input
