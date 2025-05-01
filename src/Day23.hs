module Day23 (day23) where

import Control.Monad.ST (ST, runST)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.Bits
import Data.List (foldl', foldl1')
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as M
import Data.WideWord
import Debug.Trace
import MyLib (drawArray)
import Paths_AOC2022 (getDataDir)

data Direction = North | South | West | East
  deriving (Eq, Show, Ord)

instance Enum Direction where
  fromEnum North = 0
  fromEnum South = 1
  fromEnum West = 2
  fromEnum East = 3
  toEnum n = case n `mod` 4 of
    0 -> North
    1 -> South
    2 -> West
    3 -> East

factor = 50

-- factor = 60

len = 70 + 2 * factor

type Elves = Vector Word256

readInput :: String -> Elves
readInput s = output
  where
    a = drawArray @Array (lines s)
    ls = [(y + factor, bit (len - factor - x)) | ((x, y), '#') <- A.assocs a]
    output = V.create $ do
      v <- M.replicate len 0
      mapM_ (\(i, x) -> M.modify v (.|. x) i) ls
      pure v

north, south, east, west, northeast, southeast, northwest, southwest :: Elves -> Elves
north elves0 = V.snoc (V.tail elves0) 0
south elves0 = V.cons 0 (V.init elves0)
west = V.map (`shiftL` 1)
east = V.map (`shiftR` 1)
northwest = west . north
southwest = west . south
northeast = east . north
southeast = east . south

surrounds :: Elves -> Elves
surrounds e = foldl' (\acc f -> V.zipWith (.|.) acc (f e)) (north e) [south, east, west, northeast, southeast, northwest, southwest]

step :: Direction -> Elves -> Elves
step d0 elves0 = go notMove move d0 4
  where
    n = north elves0
    s = south elves0
    w = west elves0
    e = east elves0
    hor = foldl1' (V.zipWith (.|.)) [elves0, e, w]
    ver = foldl1' (V.zipWith (.|.)) [elves0, n, s]
    surrounds = foldl1' (V.zipWith (.|.)) [e, w, north hor, south hor]
    move = V.zipWith (.&.) elves0 surrounds
    notMove = V.zipWith (\a b -> a .&. complement b) elves0 surrounds
    go acc m d n
      | n <= 0 = V.zipWith (.|.) m acc
      | move <- V.zipWith (\a b -> a .&. complement b) (f0 m) vh,
        propose <- f1 move,
        m' <- V.zipWith (\a b -> a .&. complement b) m propose,
        conflict <- V.zipWith (.&.) move acc,
        moveBack <- V.zipWith (.|.) (f0 conflict) (f1 conflict),
        acc' <- V.zipWith (\a b -> a .&. complement b) (foldl1' (V.zipWith (.|.)) [acc, move, moveBack]) conflict =
          go acc' m' (succ d) (pred n)
      where
        (f0, f1, vh) = case d of
          North -> (north, south, hor)
          South -> (south, north, hor)
          West -> (west, east, ver)
          East -> (east, west, ver)

runA n d e
  | n <= 0 = e
  | otherwise = runA (pred n) (succ d) (step d e)

runB d e = go 0 d e
  where
    go n d e
      | e' == e = n + 1
      | otherwise = go (succ n) (succ d) e'
      where
        e' = step d e

showElves :: Elves -> String
showElves elves =
  unlines
    . map (\x -> foldl' (\acc b -> if testBit x b then '#' : acc else '.' : acc) [] [b .. 256 - a])
    . V.toList
    . V.reverse
    . V.dropWhile (== 0)
    . V.reverse
    $ V.dropWhile (== 0) elves
  where
    (a, b, n) = V.foldl' (\(a, b, acc) x -> (min a (countLeadingZeros x), min b (countTrailingZeros x), acc + popCount x)) (256, 256, 0) elves

solveA :: Elves -> Int
solveA e = square - n
  where
    (a, b, n) = V.foldl' (\(a, b, acc) x -> (min a (countLeadingZeros x), min b (countTrailingZeros x), acc + popCount x)) (256, 256, 0) e
    h = V.length $ V.dropWhile (== 0) $ V.reverse $ V.dropWhile (== 0) e
    square = h * (256 - b - a)

day23 :: IO ()
day23 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  putStrLn
    . ("day23a: " ++)
    . show
    . solveA
    $ runA 10 North input
  putStrLn
    . ("day23b: " ++)
    . show
    $ runB North input
