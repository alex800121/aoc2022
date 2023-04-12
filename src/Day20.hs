module Day20 (day20) where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import MyLib

day20 :: IO ()
day20 = do
  input <- Seq.fromList . zip [0 ..] . map (read @Int) . lines <$> readFile "input20.txt"
  putStrLn $ ("day20a: " ++) $ show input
  putStrLn $ ("day20b: " ++) $ show ""
