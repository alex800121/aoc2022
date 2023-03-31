{-# LANGUAGE LambdaCase #-}
module Day16 (day16) where

import MyLib
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Maybe (isJust, fromJust)
import Control.Monad.ST (ST, runST)
import Data.List.Split
import Data.List

type Bit = Bool
one = True
zero = False
type Bits = Vector Bool

initBits :: String -> Bits
initBits = V.fromList . map (\case ; '1' -> True ; '0' -> False)

input :: String
input = "10111011111001111"

printBits :: [Bit] -> String
printBits = map (\x -> if x then '1' else '0')

buildBits :: Int -> Int -> Int -> MV.STVector s (Maybe Bool) -> ST s (Vector Bool)
buildBits refLen start len v
  | start >= len = V.map fromJust <$> V.freeze v
  | start > refLen * 2 = buildBits (refLen * 2 + 1) start len v
  | start > refLen = let n = 2 * refLen - start in MV.read v n >>= (MV.write v start . fmap not) >> buildBits refLen (start + 1) len v
  | start == refLen = MV.write v start (Just zero) >> buildBits refLen (start + 1) len v
  | start < refLen = buildBits refLen (start + 1) len v

buildBits' :: Int -> Vector Bool -> Vector Bool
buildBits' targetLen v = runST $ do
  let len = V.length v
  v' <- V.thaw (V.map Just v) >>= (`MV.grow` (targetLen - len))
  buildBits len 0 targetLen v'

checkSum :: [Bit] -> [Bit]
checkSum bits
  | odd $ length bits = bits
  | otherwise = checkSum . f $ bits
  where
    f [] = []
    f [x] = []
    f (x : y : xs) = (x == y) : f xs

day16 :: IO ()
day16 = do
  putStr "day16a: " >> putStrLn (printBits $ checkSum $ V.toList $ buildBits' 272 $ initBits input)
  putStr "day16b: " >> putStrLn (printBits $ checkSum $ V.toList $ buildBits' 35651584 $ initBits input)
