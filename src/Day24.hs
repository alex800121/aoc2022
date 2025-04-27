module Day24 (day24) where

import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl')
import Data.PQueue.Prio.Min qualified as Q
import Debug.Trace (traceShow)
import Paths_AOC2022
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as M

manhattan' (a, b) (c, d) = abs (a - c) + abs (b - d)

-- width = 120
-- height = 25

inputParser st = bl
  where
    st' = lines st
    b@((minx, miny), (maxx, maxy)) = ((1, 1), (length (head st') - 2, length st' - 2))
    lx = maxx - minx + 1
    ly = maxy - miny + 1
    bl :: Array (Int, Int) Char =
      array
        b
        [ ((x, y), c)
          | (y, l) <- zip [0 ..] st',
            (x, c) <- zip [0 ..] l,
            inRange b (x, y)
        ]

adjacents :: [Int]
adjacents = map toInt [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]

checkAvail blizzard minute i =
  (x, y) == start || (x, y) == end || (inRange b a && f x y minute)
  where
    b@((minx, miny), (maxx, maxy)) = bounds blizzard
    start = (minx, miny - 1)
    end = (maxx, maxy + 1)
    a@(x, y) = fromInt i
    width = maxx - minx + 1
    height = maxy - miny + 1
    f x y t =
      and
        [ blizzard ! ((x - t - minx) `mod` width + minx, y) /= '>',
          blizzard ! ((x + t - minx) `mod` width + minx, y) /= '<',
          blizzard ! (x, (y - t - miny) `mod` height + miny) /= 'v',
          blizzard ! (x, (y + t - miny) `mod` height + miny) /= '^'
        ]

fromInt i = (x, y)
  where
    x = i `shiftR` 5
    y = i `mod` (2 ^ 5)

toInt (x, y) = (x `shiftL` 5) + y

bfs bound blizzard ends !s !t
  | IS.null s = Nothing
  | null ends = Just t
  | e : es <- ends, IS.foldr (\x acc -> acc || e == x) False s = bfs bound blizzard es (IS.singleton e) t
  | otherwise = bfs bound blizzard ends s' (t + 1)
  where
    s' = IS.foldl' f IS.empty s
    f acc x = foldl' (g x) acc adjacents
    g x acc' y
      | checkAvail blizzard (t + 1) i = IS.insert i acc'
      | otherwise = acc'
      where
        i = x + y

solve b bl ends s = bfs b bl ends (IS.singleton s) 0

day24 :: IO ()
day24 = do
  blizzard <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  let bound@((mx, my), (w, h)) = bounds blizzard
      start = toInt (mx, my - 1)
      end = toInt (w, h + 1)
  putStrLn
    . ("day24a: " ++)
    . show
    $ solve bound blizzard [end] start
  putStrLn
    . ("day24b: " ++)
    . show
    $ solve bound blizzard [end, start, end] start
