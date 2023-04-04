module Day17 (day17) where

import MyLib
import Data.Word
import GHC.Bits
import Data.Vector (Vector)
import qualified Data.Vector as V

type Shape = [Word8]
data Board = Board { board :: [Word8], windPos :: Int, windLen :: Int, wind :: Wind }
type Wind = Vector (Shape -> Shape)

instance Show Board where
  show (Board b wp _ _) = drawShape b ++ '\n' : show wp
  
shapes :: [Shape]
shapes = [ [120], [16, 56, 16], [32, 32, 56], [8, 8, 8, 8], [24, 24] ]

drawShape :: Shape -> String
drawShape = unlines . map (\y -> map ((\x -> if x then '#' else '.') . testBit y) [1..7])

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
      | inBound s'' && all (== 0) (zipWith (.&.) s'' b') = g b' (wp' + 1 `mod` wl') wl' w' s''
      | otherwise = g b' (wp' + 1 `mod` wl') wl' w' s'
      where
        s'' = w' V.! wp' $ s'
    g b' wp' wl' w' s'
      | length s'' <= length b' && all (== 0) (zipWith (.&.) s'' b') = f b' wp' wl' w' s''
      | otherwise = Board (dropWhile (== 0) $ zipWith (.&.) s'' b') wp' wl' w'
      where
        s'' = 0 : s'

day17 :: IO ()
day17 = do
  putStrLn $ ("day17a: " ++) $ show ""
  putStrLn $ ("day17b: " ++) $ show ""
