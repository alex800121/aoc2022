{-# LANGUAGE LambdaCase #-}

module Day17 (day17) where

import Data.Bifunctor (Bifunctor (..))
import Data.List
import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as U
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import GHC.Bits
import MyLib
import Paths_AOC2022

type Shape = [Word8]

data Board = Board {board :: [Word8], windPos :: Int, shapePos :: Int, height :: Int}

type WindL = Bool

instance Show Board where
  show (Board b wp sp h) = drawShape b ++ "\nheight = " ++ show h ++ '\n' : show wp ++ '\n' : show sp

shapes :: Vector Shape
shapes = V.fromList [[120], [16, 56, 16], [32, 32, 56], [8, 8, 8, 8], [24, 24]]

drawShape :: Shape -> String
drawShape = unlines . map (\y -> map ((\x -> if x then '#' else '.') . testBit y) [1 .. 7])

inBound :: Shape -> Bool
inBound = all even

moveLeft :: Shape -> Shape
moveLeft = map (`rotateR` 1)

moveRight :: Shape -> Shape
moveRight = map (`rotateL` 1)

move True = moveLeft
move False = moveRight

nextShape :: U.Vector WindL -> Vector Shape -> Board -> Board
nextShape wv sv (Board b wp sp h) = f b' s wp h'
  where
    lenw = V.length wv
    lens = V.length sv
    s = sv V.! sp
    h' = 3 + length s
    b' = replicate h' 0 <> b
    f b s wp h''
      | all even s' && all (== 0) (zipWith (.&.) s' b) = g b s' ((wp + 1) `mod` lenw) h''
      -- | not (any (`testBit` 0) s') && all (== 0) (zipWith (.&.) s' b) = g b s' ((wp + 1) `mod` lenw) h''
      | otherwise = g b s ((wp + 1) `mod` lenw) h''
      where
        s' = move (wv U.! wp) s
    g b s wp h''
      | length s' <= length b && all (== 0) (zipWith (.&.) s' b) = f b' s'' wp (max 0 (pred h''))
      | otherwise = Board (take 100 $ zipWith (.|.) (s ++ repeat 0) b) wp (succ sp `mod` lens) (h + h'')
      where
        s' = 0 : s
        (b', s'') = if h'' > 0 then (tail b, s) else (b, s')

findCycle l = go l l 0 1 2
  where
    go (x : xs) (_ : y : ys) tu ra li
      | x == y = Just (ra - tu, tu, x)
      | tu + ra >= li = go (y : ys) (y : ys) ra (ra + 1) (li * 2)
      | otherwise = go (x : xs) (y : ys) tu (ra + 1) li
    go _ _ _ _ _ = Nothing

day17 :: IO ()
day17 = do
  wind <- U.fromList . mapMaybe (\case '>' -> Just False; '<' -> Just True; _ -> Nothing) <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let boardList = iterate (nextShape wind shapes) (Board [] 0 0 0)
      Just (diff, fi, _) = findCycle $ map ((,) <$> shapePos <*> windPos) boardList
      n = 1000000000000
      m = n - fi
      (o, p) = m `divMod` diff
      heightA = height $ boardList !! fi
      heightB = height $ boardList !! (fi + diff)
      heightC = height $ boardList !! (fi + diff + p)
      heightDiff = heightB - heightA
      heightDiff' = heightC - heightB
  -- print $ boardList !! 10
  -- print (diff, fi)
  -- print $ findCycle $ map (take 30 . board) boardList
  putStrLn $ ("day17a: " ++) $ show $ height $ boardList !! 2022
  putStrLn $ ("day17b: " ++) $ show $ heightA + o * heightDiff + heightDiff'
