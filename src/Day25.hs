{-# LANGUAGE GADTs #-}

module Day25 (day25) where

import MyLib hiding (Nat (..), intToBits)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad (when)
import Debug.Trace
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Char

type Registers = IntMap Int
type Register = Int
data RorI = R Int | I Int deriving (Show, Eq, Ord)
data Instruction where
  Cpy :: RorI -> Register -> Instruction
  Inc :: Register -> Instruction
  Dec :: Register -> Instruction
  Jnz :: RorI -> RorI -> Instruction
  Out :: RorI -> Instruction
  deriving (Show, Eq)
type Instructions = Vector Instruction
data GameState = G { step :: Int, regs :: Registers, instructions :: Instructions } deriving (Show, Eq)

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
  ( string "jnz" >> space >> ((:) <$> ((Jnz <$> roriParser <*> roriParser) <* space) <*> inputParser) ) <|>
  ( string "out" >> space >> ((:) <$> ((Out <$> roriParser) <* space) <*> inputParser) )

interpretIns :: GameState -> [Int]
interpretIns g@(G i r instructions) = case instructions Vector.!? i of
-- interpretIns g@(G i r instructions) = trace (show (i, r)) $ case instructions Vector.!? i of
  Nothing -> []
  Just ins -> case ins of
    Cpy roi reg -> interpretIns $ G (i + 1) (IntMap.insert reg (interpretRorI r roi) r) instructions
    Inc reg -> interpretIns $ G (i + 1) (IntMap.insertWith (+) reg 1 r) instructions
    Dec reg -> interpretIns $ G (i + 1) (IntMap.insertWith subtract reg 1 r) instructions
    Jnz roi1 roi2 -> interpretIns $ G (i + if interpretRorI r roi1 == 0 then 1 else interpretRorI r roi2) r instructions
    Out roi -> interpretRorI r roi : interpretIns (G (i + 1) r instructions)

bitToInteger :: String -> Int
bitToInteger = foldr (\x acc -> acc * 2 + digitToInt x) 0
intToBits :: Int -> String
intToBits = f []
  where
    f a 0 = '0' : a
    f a 1 = '1' : a
    f a n = f (intToDigit (n `mod` 2) : a) (n `div` 2)

interpretRorI :: Registers -> RorI -> Int
interpretRorI r roi = case roi of
  R i -> r IntMap.! i
  I i -> i

fib :: [Int]
fib = f 1 1
  where
    f x y = x : f y (x + y)

alternate01 :: [Integer]
alternate01 = cycle [0, 1]

convert01 :: [Integer] -> Integer
convert01 [] = 0
convert01 (x : xs) = x + 2 * convert01 xs

day25 :: IO ()
day25 = do
  -- ins <- Vector.fromList . fromJust . parseMaybe inputParser <$> readFile "input25.txt"
  putStrLn $ ("day25a: " ++) $ show $ subtract (633 * 4) $ head $ dropWhile (< 633 * 4) $ map (convert01 . (`take` alternate01) . (* 2)) [1..]
  -- print $ (\x -> interpretIns (G 0 (IntMap.insert 97 x initRegisters) ins)) (2730 - 633 * 4)