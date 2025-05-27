module Day20 (day20) where

import Control.Monad (foldM)
import Control.Monad.ST.Strict
import Data.Array (Array)
import Data.Array.IArray qualified as A
import Data.Array.MArray qualified as MA
import Data.Array.ST (STArray)
import Data.Foldable (toList)
import Data.List.Split (chunksOf)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Vector.Strict qualified as SV
import Data.Vector.Strict.Mutable qualified as SMV
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace (traceM)
import Paths_AOC2022 (getDataDir)

key = 811589153

type Input = Vector Int

type Chain s = SMV.STVector s (SMV.STVector s (Seq Int))

type Lookup s = STVector s (Int, Int)

type Jump s = STVector s Int

run round key input = runST $ do
  lookup <- MV.replicate len (0, 0)
  chain0 <- SMV.new 0
  jump0 <- MV.new 0
  let go0 chain jump l0 (x : xs) = do
        chain <- SMV.grow chain 1
        c0 <- SMV.new 0
        SMV.write chain l0 c0
        jump <- MV.grow jump 1
        MV.write jump l0 (0 :: Int)
        go1 chain jump c0 l0 xs 0 x
      go0 chain jump l0 [] = pure (chain, jump)
      go1 chain jump c l0 xs l1 [] = go0 chain jump (succ l0) xs
      go1 chain jump c l0 xs l1 (y : ys) = do
        mapM_ (\i -> MV.write lookup i (l0, l1)) y
        c <- SMV.grow c 1
        let s = S.fromList y
        SMV.write c l1 s
        SMV.write chain l0 c
        MV.modify jump (+ S.length s) l0
        go1 chain jump c l0 xs (succ l1) ys
  (chain, jump) <- go0 chain0 jump0 0 xs
  mapM_ (\_ -> mix len key lookup chain jump input') [1 .. round]
  indices <- SMV.foldM' (SMV.foldl' (<>)) S.empty chain
  let Just zeroPos = S.elemIndexL zeroIndex indices
  pure $ sum $ map (\i -> input' V.! (indices `S.index` ((i + zeroPos) `mod` len))) [1000, 2000, 3000]
  where
    len = V.length input
    xs = chunksOf 17 (chunksOf 17 [0 .. len - 1])
    Just zeroIndex = V.elemIndex 0 input
    input' = V.map (* key) input

mix :: Int -> Int -> Lookup s -> Chain s -> Jump s -> Input -> ST s ()
mix len key lookup chain jump = V.imapM_ (move len lookup chain jump)

insertNewPos :: Lookup s -> Chain s -> Jump s -> Int -> Int -> Int -> ST s ()
insertNewPos lookup chain jump index l0 curPos = do
  j0 <- MV.read jump l0
  if j0 <= curPos
    then insertNewPos lookup chain jump index (succ l0) (curPos - j0)
    else do
      MV.modify jump succ l0
      c <- SMV.read chain l0
      go lookup chain c curPos [0..SMV.length c - 1]
  where
    go :: Lookup s -> Chain s -> SMV.STVector s (Seq Int) -> Int -> [Int] -> ST s ()
    go lookup chain c curPos (l1 : xs) = do
      s <- SMV.read c l1
      let lenS = S.length s
      if lenS <= curPos
        then go lookup chain c (curPos - lenS) xs
        else MV.write lookup index (l0, l1) >> SMV.modify c (S.insertAt curPos index) l1

move :: Int -> Lookup s -> Chain s -> Jump s -> Int -> Int -> ST s ()
move len lookup chain jump index n = do
  (l0, l1) <- MV.read lookup index
  c0 <- SMV.read chain l0
  l2 <- SMV.read c0 l1
  let Just l2Pos = S.elemIndexL index l2
  SMV.modify c0 (S.deleteAt l2Pos) l1
  MV.modify jump pred l0
  len0 <- foldM (\acc v -> (acc +) <$> MV.read jump v) 0 [0 .. l0 - 1]
  len1 <- foldM (\acc v -> (acc +) . S.length <$> SMV.read c0 v) 0 [0 .. l1 - 1]
  let pos = len0 + len1 + l2Pos
      newPos = (pos + n) `mod` (len - 1)
  insertNewPos lookup chain jump index 0 newPos

day20 :: IO ()
day20 = do
  rawInput <- V.fromList . map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  -- rawInput <- V.fromList . map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/test20.txt"))
  print $ run 1 1 rawInput
  print $ run 10 key rawInput

