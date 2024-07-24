module Day7 (day7) where

import Paths_AOC2022
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (void)
import Data.List

data Entry = Dir { chlidren :: FileSystem }
           | File { size :: Int }
  deriving (Show, Eq, Ord)
type FileSystem = Map String Entry

calcFS :: FileSystem -> Int
calcFS = sum . Map.map calcEntry

calcEntry :: Entry -> Int
calcEntry (File i) = i
calcEntry (Dir fs) = calcFS fs

traverseFolderSize :: FileSystem -> [(String, Int)]
traverseFolderSize = concatMap f . Map.toList
  where
    f (_, File _) = []
    f (s, Dir d) = (s, calcFS d) : traverseFolderSize d

unionEntry :: Entry -> Entry -> Entry
unionEntry (File a) (File b) = File (max a b)
unionEntry (Dir a) (Dir b) = Dir (Map.union a b)
unionEntry a b = error ("Unmatched entries: " ++ show (a, b))

readFileSystem :: Parser FileSystem
readFileSystem = ((void (string "$ cd .." <* space) <|> eof) >> pure Map.empty) <|> do
  string "$ cd "
  name <- some (satisfy (not . isSpace)) <* space
  children <- fromMaybe Map.empty <$> optional (string "$ ls" >> space >> readLS)
  children' <- readFileSystem
  Map.insertWith unionEntry name (Dir (Map.unionWith unionEntry children children')) <$> readFileSystem

readLS :: Parser FileSystem
readLS = (eof >> pure Map.empty) <|>
  ( do
      n <- signedInteger <* space
      name <- some (satisfy (not . isSpace))
      optional space
      Map.insert name (File n) <$> readLS
  ) <|>
  ( do
      string "dir "
      name <- some (satisfy (not . isSpace))
      optional space
      Map.insert name (Dir Map.empty) <$> readLS
  ) <|> pure Map.empty

printLevel :: Int -> FileSystem -> String
printLevel n = unlines . map f . Map.toList
  where
    f (s, File i) = replicate n ' ' ++ show i ++ ' ' : s
    f (s, Dir d) = replicate n ' ' ++ "dir " ++ s ++ '\n' : printLevel (n + 4) d

day7 :: IO ()
day7 = do
  fs <- fromJust . parseMaybe readFileSystem <$> (getDataDir >>= readFile . (++ "/input/input7.txt")) 
  let total = 70000000
      need = 30000000
      upper = total - need
      fsSize = traverseFolderSize fs
      rootSize = snd $ fromJust $ find ((== "/") . fst) fsSize
      l = head $ dropWhile ((> upper) . (rootSize -)) $ sort $ map snd fsSize
  putStrLn $ ("day7a: " ++ ) $ show $ sum $ filter (<= 100000) $ map snd fsSize
  putStrLn $ ("day7b: " ++ ) $ show l
  -- putStrLn $ ("day7b: " ++ ) $ show $ map supportsSSL input
