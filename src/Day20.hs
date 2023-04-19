module Day20 (day20) where

import Data.List
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import MyLib

day20 :: IO ()
day20 = do
  -- input <- Seq.fromList . zip [0 ..] . map (read @Int) . lines <$> readFile "test20.txt"
  input <- Seq.fromList . zip [0 ..] . map (read @Int) . lines <$> readFile "input20.txt"
  let len = length input
  -- mapM_ print $ scanl' (step len) input [0 .. len - 1]
  let f x = let Just n = Seq.findIndexL ((== 0) . snd) x in map (snd . Seq.index x . (`mod` length x) . (+ n)) [1000, 2000, 3000]
      g x = x * 811589153
      input' = fmap g <$> input
  putStrLn $ ("day20a: " ++) $ show $ sum $ f $ foldl' (step len) input [0 .. len - 1]
  putStrLn $ ("day20b: " ++) $ show $ sum $ f $ foldl' (step len) input' $ take (len * 10) $ cycle [0 .. len - 1]

step :: Int -> Seq (Int, Int) -> Int -> Seq (Int, Int)
step len s i
  | a == 0 = s
  | a < 0 = Seq.insertAt newIndex' n $ Seq.deleteAt i' s
  | otherwise = Seq.insertAt newIndex n $ Seq.deleteAt i' s
  where
    i' = fromJust (Seq.findIndexL ((== i) . fst) s)
    n@(_, a) = Seq.index s i'
    newIndex = (a + i') `mod` (len - 1)
    newIndex' = ((a + i' - 1) `mod` (len - 1)) + 1
