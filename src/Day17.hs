module Day17 (day17) where

import MyLib
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16
import Data.Set (Set)
import Crypto.Hash.MD5
import qualified Data.Set as Set
import Debug.Trace

type Index = (Int, Int)
type Bounds = (Index, Index)
data GameState = G { pos :: Index, des :: Index, pw :: ByteString, path :: ByteString }
  deriving (Show, Eq)

instance Ord GameState where
  compare (G p1 d1 pw1 s1) (G p2 d2 pw2 s2) =
       compare (BS.length s1 + manhattan' p1 d1) (BS.length s2 + manhattan' p2 d2)
    <> compare (BS.length s1) (BS.length s2)
    <> compare (manhattan' p1 d1) (manhattan' p2 d2)
    <> compare s1 s2
    <> compare p2 p1
    <> compare pw1 pw2
    <> compare d1 d2

manhattan' :: Num a => (a, a) -> (a, a) -> a
manhattan' (a, b) (c, d) = abs (a - c) + abs (b - d)

withinBounds :: Bounds -> Index -> Bool
withinBounds ((minX, minY), (maxX, maxY)) (x, y) =
     minX <= x
  && maxX >= x
  && minY <= y
  && maxY >= y

choices :: Bounds -> GameState -> Set GameState
choices bounds g@(G _ _ pw path) = Set.filter (withinBounds bounds . (.pos)) $ Set.map (f g) next
  where
    next = Set.fromList $ map fst $ filter snd $ zip "UDLR" $ map (`elem` "bcdef") $ take 4 $ BS.unpack $ encode $ hash $ pw <> path
    f (G pos' des' pw' path') x = G (pos' +& y) des' pw' (BS.snoc path' x)
      where
        y = case x of
          'U' -> (0, -1)
          'D' -> (0, 1)
          'L' -> (-1, 0)
          'R' -> (1, 0)
    

aStar :: Bounds -> Set GameState -> GameState
aStar bounds pool
  -- | trace (show picked) False = undefined
  | pos picked == des picked = picked
  | otherwise = aStar bounds pool'
  where
    (picked, rest) = Set.deleteFindMin pool
    next = choices bounds picked
    pool' = Set.union next rest

bfs :: Bounds -> Set GameState -> Set GameState -> Set GameState
bfs bounds pool reached
  | Set.null pool = reached
  | otherwise = bfs bounds next (Set.union done reached)
  where
    (done, notDone) = Set.partition (\x -> x.pos == x.des) pool
    next = Set.unions $ Set.map (choices bounds) notDone
    
input :: ByteString
input = BS.pack "pxxbnzuo"

initGameState :: GameState
initGameState = G (0, 0) (3, 3) input BS.empty

initBounds :: Bounds
initBounds = ((0, 0), (3, 3))

test :: ByteString
test = BS.pack "ulqzkmiv"

day17 :: IO ()
day17 = do
  putStrLn $ ("day17a: " ++) $ BS.unpack $ path $ aStar initBounds (Set.singleton initGameState)
  putStrLn $ ("day17b: " ++) $ show $ maximum $ Set.map (BS.length . path) $ bfs initBounds (Set.singleton initGameState) Set.empty
