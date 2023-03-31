module Day7 (day7) where

import MyLib
import Data.List.Split
import Data.List
import Text.Megaparsec
import Data.Maybe (fromJust)

fstParser :: Parser ([String], [String])
fstParser = (eof >> return ([], [])) <|> do
  a <- many (anySingleBut '[')
  mapFirst (a :) <$> sndParser

sndParser :: Parser ([String], [String])
sndParser = (eof >> return ([], [])) <|> do
  a <- many (anySingleBut ']')
  fmap (a :) <$> fstParser

hasABBA :: String -> Bool
hasABBA = any (\x -> head x == x !! 3 && x !! 1 == x !! 2 && x !! 1 /= head x) . divvy 4 1

supportsTLS :: ([String], [String]) -> Bool
supportsTLS (xs, ys) = any hasABBA xs && not (any hasABBA ys)

supportsSSL :: ([String], [String]) -> Bool
supportsSSL (xs, ys) = (not . null) (bab `intersect` bab')
  where
    aba = concatMap (filter (\x -> head x == x !! 2 && head x /= x !! 1) . divvy 3 1) xs
    bab = concatMap (filter (\x -> head x == x !! 2 && head x /= x !! 1) . divvy 3 1) ys
    bab' = map (\x -> x !! 1 : take 2 x) aba

day7 :: IO ()
day7 = do
  input <- map (fromJust . parseMaybe fstParser) . lines <$> readFile "input7.txt"
  putStrLn $ ("day7a: " ++ ) $ show $ length $ filter supportsTLS input
  putStrLn $ ("day7b: " ++ ) $ show $ length $ filter supportsSSL input
  -- putStrLn $ ("day7b: " ++ ) $ show $ map supportsSSL input
