{-# LANGUAGE LambdaCase #-}

module Day22 (day22) where

import Paths_AOC2022
import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import MyLib hiding (toIndex)
import Text.Megaparsec
import Text.Megaparsec.Char

type Ins = Either Int Char

type Index = (Int, Int)

type CubeIndex = (Index, Index)

type CubeSide = Map Index (Map Direction (Index, Direction))

type Bot a = (a, Direction)

type Plane = Map Index Bool

type Cube = Map CubeIndex Bool

insParser :: Parser [Ins]
insParser = (eof >> pure []) <|> ((:) <$> ((Left <$> signedInteger <* space) <|> (Right <$> anySingle <* space)) <*> insParser)

step :: Plane -> Bot Index -> Ins -> Bot Index
step p (i, d) (Left n)
  | n <= 0 = (i, d)
  | otherwise = if next == i then (i, d) else step p (next, d) (Left (n - 1))
  where
    next = moveTo p i d
step p (i, d) (Right 'R') = (i, succ d)
step p (i, d) (Right 'L') = (i, pred d)

step' :: Int -> CubeSide -> Cube -> Bot CubeIndex -> Ins -> Bot CubeIndex
step' len cs c bot (Left n)
  | n <= 0 = bot
  | otherwise = if next == bot then bot else step' len cs c next (Left (n - 1))
  where
    next = moveTo' len cs c bot
step' _ _ _ (x, d) (Right 'R') = (x, succ d)
step' _ _ _ (x, d) (Right 'L') = (x, pred d)

moveTo' :: Int -> CubeSide -> Cube -> Bot CubeIndex -> Bot CubeIndex
moveTo' len cs c ((ci, (x, y)), d) = f (ci, next) d
  where
    (next, next') = case d of
      North -> ((x, y - 1), [(x, len - 1), (0, x), (len - 1 - x, 0), (len - 1, len - 1 - x)])
      East -> ((x + 1, y), [(y, len - 1), (0, y), (len - 1 - y, 0), (len - 1, len - 1 - y)])
      South -> ((x, y + 1), [(len - 1 - x, len - 1), (0, len - 1 - x), (x, 0), (len - 1, x)])
      West -> ((x - 1, y), [(len - 1 - y, len - 1), (0, len - 1 - y), (y, 0), (len - 1, y)])
    (ci', d') = (cs Map.! ci) Map.! d
    f n m = case c Map.!? n of
      Nothing -> f (ci', next' !! fromEnum d') d'
      Just True -> (n, m)
      Just False -> ((ci, (x, y)), d)

moveTo :: Plane -> Index -> Direction -> Index
moveTo p (x, y) d = case d of
  North -> f (x, y - 1) maximum ((== x) . fst)
  South -> f (x, y + 1) minimum ((== x) . fst)
  East -> f (x + 1, y) minimum ((== y) . snd)
  West -> f (x - 1, y) maximum ((== y) . snd)
  where
    f (a, b) g h = case p Map.!? (a, b) of
      Nothing -> let (a', b') = g $ filter h $ Map.keys p in f (a', b') g h
      Just True -> (a, b)
      Just False -> (x, y)

password :: Bot Index -> Int
password ((x, y), d) = (y + 1) * 1000 + (x + 1) * 4 + ((fromEnum d + 3) `mod` 4)

toCubeIndex :: Int -> Index -> CubeIndex
toCubeIndex l (x, y) = ((x `div` l, y `div` l), (x `mod` l, y `mod` l))

toIndex :: Int -> CubeIndex -> Index
toIndex l ((a, b), (c, d)) = (a * l + c, b * l + d)

planeToInitCube :: Plane -> CubeSide
planeToInitCube l =
  Map.fromList
    [ (x, Map.fromList $ mapMaybe (f x) l')
      | let l' = map (fst . toCubeIndex (sideLen l)) $ Map.keys l,
        let f a b = case a -& b of
              (0, 1) -> Just (North, (b, North))
              (0, -1) -> Just (South, (b, South))
              (1, 0) -> Just (West, (b, West))
              (-1, 0) -> Just (East, (b, East))
              _ -> Nothing,
        x <- l'
    ]

foldCube :: CubeSide -> CubeSide
foldCube c = if c == c' then c else foldCube c'
  where
    f l =
      Map.unionsWith Map.union $
        concat $
          [ mapMaybe (g x) l
            | x <- l
          ]
    c' = Map.unionWith Map.union c $ Map.unionsWith Map.union $ map (f . Map.toList) $ Map.elems c
    g (dx, (ix, dx')) (dy, (iy, dy'))
      | succ dx == dy = Just $ Map.singleton ix $ Map.singleton (succ dx') (iy, succ dy')
      | pred dx == dy = Just $ Map.singleton ix $ Map.singleton (pred dx') (iy, pred dy')
      | otherwise = Nothing

toCube :: Plane -> Cube
toCube p = Map.mapKeys (toCubeIndex l) p
  where
    l = sideLen p

sideLen :: Plane -> Int
sideLen = round . sqrt . fromIntegral . (`div` 6) . length

day22 :: IO ()
day22 = do
  -- (instructions, plane) <- (\(y : x : _) -> (fromJust $ parseMaybe insParser x, drawMap (\case ' ' -> Nothing; '.' -> Just True; '#' -> Just False) $ lines y)) . splitOn "\n\n" <$> readFile "test22.txt"
  (instructions, plane) <- (\(y : x : _) -> (fromJust $ parseMaybe insParser x, drawMap (\case ' ' -> Nothing; '.' -> Just True; '#' -> Just False) $ lines y)) . splitOn "\n\n" <$>(getDataDir >>= readFile . (++ "/input/input22.txt")) 
  let startIndex = minimumBy (\x y -> (compare `on` snd) x y <> (compare `on` fst) x y) $ Map.keys plane
      initBot = (startIndex, East)
      cubeSide = foldCube $ planeToInitCube plane
      cube = toCube plane
      l = sideLen plane
      initBot' = (toCubeIndex l startIndex, East)
  putStrLn $ ("day22a: " ++) $ show $ password $ foldl' (step plane) initBot instructions
  putStrLn $ ("day22b: " ++) $ show $ password $ mapFirst (toIndex l) $ foldl' (step' l cubeSide cube) initBot' instructions
