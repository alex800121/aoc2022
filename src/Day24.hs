{-# LANGUAGE LambdaCase #-}

module Day24 (day24) where

import MyLib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace

type Index = (Int, Int)
data Spot = Open | Wall | Number Int
  deriving (Show, Ord, Eq)
type Duct = Map Index Spot
data GameState = G { pending :: Set Int, steps :: Int, start :: Int, dm :: DistanceMap, backToZero :: Bool }
  deriving (Show, Eq)
instance Ord GameState where
  compare (G p1 s1 st1 dm1 b1) (G p2 s2 st2 dm2 b2) = 
       compare s1 s2
    <> compare (length p1) (length p2)
    <> compare b2 b1
    <> compare p1 p2
    <> compare st1 st2
    <> compare dm1 dm2
type DistanceMap = Map Int (Map Int Int)

inputParser :: String -> Duct
inputParser = drawMap f . lines
  where
    f '.' = Just Open
    f '#' = Nothing
    f x = Just (Number (digitToInt x))

buildDistanceMap :: Duct -> DistanceMap
buildDistanceMap d = Map.unionsWith (Map.unionWith min) $ map f nums
  where
    nums = mapMaybe uncons $ tails $ Map.toList $ Map.mapMaybe (\case ; Number x -> Just x ; _ -> Nothing) d
    f ((startIndex, startNum), rest) = bfs d Set.empty (Set.singleton startIndex) (Set.fromList $ map snd rest) startNum 0 Map.empty

adjacent :: Set Index
adjacent = Set.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

bfs :: Duct -> Set Index -> Set Index -> Set Int -> Int -> Int -> DistanceMap -> DistanceMap
bfs duct visited startIndex notVisited startNum steps acc
  | null startIndex || null notVisited = acc
  | otherwise = bfs duct visited' startIndex' notVisited' startNum steps' acc'
  where
    (added, rest) =
      mapFirst (Set.map ((\case ; Number x -> x) . (duct Map.!))) $
      Set.partition ((\case ; Number x -> x /= startNum ; _ -> False) . (duct Map.!)) startIndex
    acc' =
      Set.foldl'
        (\a x -> Map.insertWith (Map.unionWith min) startNum (Map.singleton x steps) $
                 Map.insertWith (Map.unionWith min) x (Map.singleton startNum steps) a
        )
        acc
        added
    visited' = Set.union visited startIndex
    startIndex' = Set.filter (`Map.member` duct) (Set.unions $ Set.map (\x -> Set.map (+& x) adjacent) rest) Set.\\ visited
    notVisited' = notVisited Set.\\ added
    steps' = steps + 1

bfs' :: Duct -> Set Index -> Set Index -> Int -> Int -> Int
bfs' duct visited startIndex destination steps
  | any ((== Number destination) . (duct Map.!)) startIndex = steps
  | otherwise = bfs' duct (Set.union visited startIndex) startIndex' destination steps + 1
  where
    startIndex' = Set.filter (`Map.member` duct) $ Set.unions (Set.map (\x -> Set.map (+& x) adjacent) startIndex) Set.\\ visited
  
dijkstra :: Duct -> DistanceMap -> Set (Int, Set Int) -> Set GameState -> GameState
dijkstra duct dm paths s
  | null (pending picked) && backToZero picked = picked
  | null (pending picked) && not (backToZero picked) = dijkstra duct dm paths s''
  | otherwise = dijkstra duct dm paths' s'
  where
    (picked, rest) = Set.deleteFindMin s
    added = Set.filter (\x -> (start x, pending x) `Set.notMember` paths) $ choices picked
    paths' = Set.insert (start picked, pending picked) paths
    s' = Set.union added rest
    n = bfs' duct Set.empty startIndex' 0 0
    startIndex' = Set.singleton $ fst $ Map.findMin $ Map.filter (== Number (start picked)) duct
    s'' = Set.insert (picked { steps = steps picked + n, backToZero = True }) rest

choices :: GameState -> Set GameState
choices (G pending steps start dm b) = Set.map fromJust $ Set.filter isJust $ Set.map f pending
  where
    f x = dm Map.!? start >>= (Map.!? x) >>= \y -> pure (G (Set.delete x pending) (steps + y) x dm b)
    
day24 :: IO ()
day24 = do
  duct <- inputParser <$> readFile "input24.txt"
  let dm = buildDistanceMap duct
      initGameState = G (Set.delete 0 $ Map.keysSet dm) 0 0 dm True
      initGameState' = initGameState { backToZero = False }
  putStrLn $ ("day24a: " ++) $ show $ steps $ dijkstra duct dm Set.empty (Set.singleton initGameState)
  putStrLn $ ("day24b: " ++) $ show $ steps $ dijkstra duct dm Set.empty (Set.singleton initGameState')
