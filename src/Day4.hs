module Day4 (day4) where

import Data.List.Split
import Data.List
import Data.Function
import Debug.Trace

data Password = P { strings :: [String], secId :: Int, checksum :: String }
  deriving (Show, Eq, Ord)

testInput = map parseInput
  [ "aaaaa-bbb-z-y-x-123[abxyz]"
  , "a-b-c-d-e-f-g-h-987[abcde]"
  , "not-a-real-room-404[oarel]"
  , "totally-real-room-200[decoy]"
  ]
parseInput :: String -> Password
parseInput l = P st nu ch
  where
    xs = splitOn "-" l
    st = init xs
    [y, z] = splitOn "[" $ last xs
    nu = read y 
    ch = init z

check :: Password -> Bool
check (P st si ch) = ch == ch'
-- check (P st si ch) = trace (show (st''', ch, ch == ch')) ch == ch'
  where
    ch' =
      take 5 $
      map snd st'
    st' = 
      sortBy ( \x y ->
                  (compare `on` fst) y x <> (compare `on` snd) x y
             ) st'''
    st''' = map (\x -> (length x, head x)) st''
    st'' = 
      group $
      sort $
      concat st

decrypt :: Password -> Password
decrypt (P st nu ch) = P st' nu ch
  where
    st' = map f st
    m = nu `mod` 26
    f = map (toEnum . (+ 97) . (`mod` 26) . (+ m) . subtract 97 . fromEnum)

day4 :: IO ()
day4 = do
  input <- filter check . map parseInput . lines <$> readFile "input4.txt"
  -- putStrLn ("day4a: " ++ show (sum $ map secId $ filter check testInput))
  putStrLn ("day4a: " ++ show (sum $ map secId input))
  putStrLn ("day4b: " ++ show ( secId
                              $ head
                              $ filter ((\x -> all ($ x) [ elem "northpole"
                                                         , elem "object"
                                                         , elem "storage"
                                                         ]) . strings) $ map decrypt input))
