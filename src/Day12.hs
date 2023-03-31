{-# LANGUAGE GADTs #-}
module Day12 (day12) where

import MyLib hiding (Nat (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad (when)
import Debug.Trace

type Registers = IntMap Int
type Register = Int
data RorI = R Int | I Int deriving (Show, Eq, Ord)
data Instruction where
  Cpy :: RorI -> Register -> Instruction
  Inc :: Register -> Instruction
  Dec :: Register -> Instruction
  Jnz :: RorI -> RorI -> Instruction
  deriving (Show, Eq)
type Instructions = Vector Instruction
data GameState = G { step :: Int, regs :: Registers, insss :: Instructions } deriving (Show, Eq)

initRegisters :: Registers
initRegisters = IntMap.fromList [(ord x, 0) | x <- ['a' .. 'd']]

registerParser :: Parser Register
registerParser = ord <$> anySingle <* space

roriParser :: Parser RorI
roriParser = (I <$> signedInteger <* space) <|> (R <$> registerParser)

inputParser :: Parser [Instruction]
inputParser =
  ( eof >> pure [] ) <|>
  ( string "cpy" >> space >> ((:) <$> ((Cpy <$> roriParser <*> registerParser) <* space) <*> inputParser) ) <|>
  ( string "inc" >> space >> ((:) <$> ((Inc <$> registerParser) <* space) <*> inputParser) ) <|>
  ( string "dec" >> space >> ((:) <$> ((Dec <$> registerParser) <* space) <*> inputParser) ) <|>
  ( string "jnz" >> space >> ((:) <$> ((Jnz <$> roriParser <*> roriParser) <* space) <*> inputParser) )

interpretIns :: GameState -> GameState
interpretIns g@(G i r inss) = case inss Vector.!? i of
-- interpretIns g@(G i r inss) = trace (show (i, r)) $ case inss Vector.!? i of
  Nothing -> g
  Just ins -> case ins of
    Cpy roi reg -> G (i + 1) (IntMap.insert reg (interpretRorI r roi) r) inss
    Inc reg -> G (i + 1) (IntMap.insertWith (+) reg 1 r) inss
    Dec reg -> G (i + 1) (IntMap.insertWith subtract reg 1 r) inss
    Jnz roi1 roi2 -> G (i + if interpretRorI r roi1 == 0 then 1 else interpretRorI r roi2) r inss

interpretRorI :: Registers -> RorI -> Int
interpretRorI r roi = case roi of
  R i -> r IntMap.! i
  I i -> i

fib :: [Int]
fib = f 1 1
  where
    f x y = x : f y (x + y)
    
day12 :: IO ()
day12 = do
  ins <- Vector.fromList . fromJust . parseMaybe inputParser <$> readFile "input12.txt"
  let initGameState = G 0 initRegisters ins
      mockRegisters m n = IntMap.fromList [(97, fib !! n), (98, 0), (99, fib !! (n - 1)), (100, m - n)] 
      mockGameState = G 13 (mockRegisters 28 27) ins
  --(13,fromList [(97,21),(98,0),(99,13),(100,21)])
      initGameState' = G 0 (IntMap.insert 99 1 initRegisters) ins
      mockGameState' = G 13 (mockRegisters 35 34) ins
  --(13,fromList [(97,233),(98,0),(99,144),(100,23)])
  putStrLn $ ("day12a: " ++) $ show $ (IntMap.! 97) $ regs $ snd $ fromJust $ firstRepeat $ iterate interpretIns mockGameState
  putStrLn $ ("day12b: " ++) $ show $ (IntMap.! 97) $ regs $ snd $ fromJust $ firstRepeat $ iterate interpretIns mockGameState'
