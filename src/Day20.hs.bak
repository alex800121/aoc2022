{-# LANGUAGE MultiWayIf #-}

module Day20 (day20) where

import Control.Monad (replicateM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.List (elemIndex)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as M
import Paths_AOC2022 (getDataDir)

mix :: Vector Int -> STVector s Int -> STVector s Int -> ST s ()
mix input idToPos posToId = V.imapM_ (go idToPos posToId) input
  where
    len = V.length input
    go idToPos posToId i x = do
      curPos <- M.read idToPos i
      let newPos = ((`mod` (len - 1)) . (+ x)) curPos
      if
        | curPos < newPos ->
            mapM_
              ( \pos -> do
                  j <- M.read posToId (pos + 1)
                  M.modify idToPos (subtract 1) j
                  M.write posToId pos j
              )
              [curPos .. newPos - 1]
        | newPos < curPos ->
            mapM_
              ( \pos -> do
                  j <- M.read posToId (pos - 1)
                  M.modify idToPos (+ 1) j
                  M.write posToId pos j
              )
              [curPos, curPos - 1 .. newPos + 1]
        | otherwise -> pure ()
      M.write idToPos i newPos
      M.write posToId newPos i

solveB i input v = runST $ do
  idToPos <- V.thaw v
  posToId <- V.thaw v
  replicateM_ i ((\_ -> mix input idToPos posToId) ())
  (,) <$> V.freeze posToId <*> V.freeze idToPos

solve x input (posToId, idToPos) =
  sum
    [ input V.! i
      | j <- [1000, 2000, 3000],
        let i = posToId V.! (((idToPos V.! x) + j) `mod` V.length input)
    ]

day20 :: IO ()
day20 = do
  rawInput <- map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  let input = V.fromList rawInput
      Just zero = elemIndex 0 rawInput
      inputB = V.map (* 811589153) input
  putStrLn
    . ("day20a: " ++)
    . show
    . solve zero input
    $ solveB 1 input (V.generate (V.length input) id)
  putStrLn
    . ("day20b: " ++)
    . show
    . solve zero inputB
    $ solveB 10 inputB (V.generate (V.length input) id)
