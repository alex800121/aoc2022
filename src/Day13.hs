module Day13 (day13) where

import MyLib hiding (manhattan, Nat(..))
import GHC.Bits
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function
import Debug.Trace

type Index = (Int, Int)
data Step = S { start :: Index, end :: Index, steps :: Int } deriving (Show, Eq)
instance Ord Step where
  compare (S s1 e1 t1) (S s2 e2 t2) = compare (m1 + t1) (m2 + t2) <> compare t1 t2 <> compare m1 m2 <> compare s2 s1 <> compare e1 e2
    where
      m1 = manhattan s1 e1
      m2 = manhattan s2 e2

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

isOpen :: Int -> Index -> Bool
isOpen n (x, y) = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + n

adjacents :: Set Index
adjacents = Set.fromList [(0, 1), (0, -1), (-1, 0), (1, 0)]

choices :: Int -> Index -> Set Index
choices n i = Set.filter (\(x, y) -> x >= 0 && y >= 0) $ Set.filter (isOpen n) $ Set.map (+& i) adjacents

aStar :: Int -> Set Index -> Set Step -> Step
-- aStar n travelled s = trace (show (start, end)) $ if start == end then picked else aStar n (Set.insert start travelled) s'
aStar n travelled s = if start == end then picked else aStar n (Set.insert start travelled) s'
  where
    (picked@(S start end steps), rest) = Set.deleteFindMin s
    s' = Set.union rest $ Set.map (\x -> S x end (steps + 1)) $ (Set.\\ travelled) $ choices n start

bfs :: Int -> Int -> Set Index -> Set Index -> Int
bfs n limit travelled starts
  | limit <= 0 || Set.null starts = Set.size travelled + Set.size starts
  | otherwise = bfs n (limit - 1) travelled' starts'
  where
    travelled' = Set.union starts travelled
    starts' = (Set.\\ travelled) $ Set.unions $ Set.map (choices n) starts

input :: Int
input = 1352

inputEnd :: Index
inputEnd = (31, 39)

inputStart :: Index
inputStart = (1, 1)

initStep :: Step
initStep = S inputStart inputEnd 0

day13 :: IO ()
day13 = do
  putStrLn $ ("day13a: " ++) $ show $ steps $ aStar input Set.empty (Set.singleton initStep)
  putStrLn $ ("day13b: " ++) $ show $ bfs input 50 Set.empty (Set.singleton inputStart)
