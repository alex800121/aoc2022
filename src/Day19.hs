module Day19 (day19) where

import MyLib hiding (Nat (..))
import Data.Sequence
import qualified Data.Sequence as Seq
import Debug.Trace

input :: Int
input = 3018458

day19a :: Seq x -> Seq x -> x
day19a (x :<| Empty) Empty = x
day19a acc Empty = day19a Empty acc
day19a acc (x :<| Empty) = day19a Empty (x :<| acc)
day19a acc (x :<| y :<| xs) = day19a (acc :|> x) xs

day19b :: Show x => Seq x -> x
-- day19b xs | trace (show xs) False = undefined
day19b (x :<| Empty) = x
day19b (x :<| xs) = day19b (xs' :|> x)
  where
    l = Seq.length xs + 1
    n = l `div` 2
    xs' = deleteAt (n - 1) xs


day19 :: IO ()
day19 = do
  putStrLn $ ("day19a: " ++) $ show $ day19a Empty $ fromList [1..input]
  putStrLn $ ("day19b: " ++) $ show $ day19b $ fromList [1..input] 
