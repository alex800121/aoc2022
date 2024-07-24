{-# LANGUAGE LambdaCase #-}

module Day17 (day17) where

import Data.List
import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import GHC.Bits
import MyLib
import Paths_AOC2022

type Shape = [Word8]

data Board = Board {board :: [Word8], windPos :: Int, windLen :: Int, wind :: Wind}

type Wind = Vector (Shape -> Shape)

instance Show Board where
  show (Board b wp _ _) = drawShape b ++ '\n' : show wp

shapes :: [Shape]
shapes = [[120], [16, 56, 16], [32, 32, 56], [8, 8, 8, 8], [24, 24]]

drawShape :: Shape -> String
drawShape = unlines . map (\y -> map ((\x -> if x then '#' else '.') . testBit y) [1 .. 7])

inBound :: Shape -> Bool
inBound = all even

moveLeft :: Shape -> Shape
moveLeft = map (`rotateR` 1)

moveRight :: Shape -> Shape
moveRight = map (`rotateL` 1)

nextShape :: Board -> Shape -> Board
nextShape (Board b wp wl w) s = f (replicate (3 + length s) 0 ++ b) wp wl w s
  where
    f b' wp' wl' w' s'
      -- \| trace (drawShape (zipWith (.|.) (s' ++ repeat 0) b')) False = undefined
      | inBound s'' && all (== 0) (zipWith (.&.) s'' b') = g b' ((wp' + 1) `mod` wl') wl' w' s''
      | otherwise = g b' ((wp' + 1) `mod` wl') wl' w' s'
      where
        s'' = w' V.! wp' $ s'
    g b' wp' wl' w' s'
      -- \| trace (drawShape (zipWith (.|.) (s' ++ repeat 0) b')) False = undefined
      | length s'' <= length b' && all (== 0) (zipWith (.&.) s'' b') = f b' wp' wl' w' s''
      | otherwise = Board (dropWhile (== 0) $ zipWith (.|.) (s' ++ repeat 0) b') wp' wl' w'
      where
        s'' = 0 : s'

day17 :: IO ()
day17 = do
  w <- V.fromList . mapMaybe (\case '>' -> Just moveRight; '<' -> Just moveLeft; _ -> Nothing) <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  -- let w = V.fromList . map (\case ; '>' -> moveRight ; '<' -> moveLeft) $ ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  let boardList = scanl nextShape (Board [] 0 (length w) w) $ cycle shapes
      Just (_, x) = firstRepeat $ map (take 50 . board) boardList
      [a, b] = take 2 $ findIndices ((== x) . take 50 . board) boardList
      diff = b - a
      n = 1000000000000
      m = n - a
      o = m `div` diff
      p = m `mod` diff
      heightA = length $ board $ boardList !! a
      heightB = length $ board $ boardList !! b
      heightC = length $ board $ boardList !! (b + p)
      heightDiff = heightB - heightA
      heightDiff' = heightC - heightB
  putStrLn $ ("day17a: " ++) $ show $ length $ board $ boardList !! 2022
  putStrLn $ ("day17b: " ++) $ show $ heightA + o * heightDiff + heightDiff'
