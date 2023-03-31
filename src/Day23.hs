{-# LANGUAGE GADTs #-}
module Day23 (day23) where


import MyLib hiding (Nat (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import Control.Monad (when)
import Debug.Trace

type Registers = IntMap Int
type Register = Int
data RorI = R Int | I Int deriving (Show, Eq, Ord)
data Instruction where
  Cpy :: RorI -> RorI -> Instruction
  Inc :: RorI -> Instruction
  Dec :: RorI -> Instruction
  Jnz :: RorI -> RorI -> Instruction
  Tgl :: RorI -> Instruction
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
  ( string "cpy" >> space >> ((:) <$> ((Cpy <$> roriParser <*> roriParser) <* space) <*> inputParser) ) <|>
  ( string "inc" >> space >> ((:) <$> ((Inc <$> roriParser) <* space) <*> inputParser) ) <|>
  ( string "dec" >> space >> ((:) <$> ((Dec <$> roriParser) <* space) <*> inputParser) ) <|>
  ( string "jnz" >> space >> ((:) <$> ((Jnz <$> roriParser <*> roriParser) <* space) <*> inputParser) ) <|>
  ( string "tgl" >> space >> ((:) <$> (Tgl <$> roriParser <* space) <*> inputParser) )

interpretIns :: GameState -> GameState
-- interpretIns g@(G i r inss) = case inss Vector.!? i of
interpretIns g@(G i r inss) = trace (show (i, r)) $ case inss Vector.!? i of
  Nothing -> g
  Just ins -> case ins of
    Cpy roi1 roi2 -> case roi2 of
      I _ -> G (i + 1) r inss
      R reg -> G (i + 1) (IntMap.insert reg (interpretRorI r roi1) r) inss
    Inc reg -> case reg of
      I _ -> G (i + 1) r inss
      R reg -> G (i + 1) (IntMap.insertWith (+) reg 1 r) inss
    Dec reg -> case reg of
      I _ -> G (i + 1) r inss
      R reg -> G (i + 1) (IntMap.insertWith subtract reg 1 r) inss
    Jnz roi1 roi2 -> G (i + if interpretRorI r roi1 == 0 then 1 else interpretRorI r roi2) r inss
    Tgl roi -> let i' = i + interpretRorI r roi in G (i + 1) r $
      if i' < length inss && i >= 0 then Vector.modify (\v -> MV.modify v toggle (i + interpretRorI r roi)) inss else inss
    
toggle :: Instruction -> Instruction
toggle (Inc x) = Dec x
toggle (Dec x) = Inc x
toggle (Tgl x) = Inc x
toggle (Jnz x y) = Cpy x y
toggle (Cpy x y) = Jnz x y

interpretRorI :: Registers -> RorI -> Int
interpretRorI r roi = case roi of
  R i -> r IntMap.! i
  I i -> i

fib :: [Int]
fib = f 1 1
  where
    f x y = x : f y (x + y)
    
day23 :: IO ()
day23 = do
  -- ins <- Vector.fromList . fromJust . parseMaybe inputParser <$> readFile "test23.txt"
  ins <- Vector.fromList . fromJust . parseMaybe inputParser <$> readFile "input23.txt"
  -- putStrLn $ ("day22a: " ++) $ show $ firstRepeat $ iterate interpretIns (G 0 (IntMap.insert 97 7 initRegisters) ins)
  putStrLn $ ("day22a: " ++) $ show $ product [1..7] + (98 * 86)
  -- putStrLn $ ("day22b: " ++) $ show $ firstRepeat $ iterate interpretIns (G 0 (IntMap.insert 97 12 initRegisters) ins)
  putStrLn $ ("day23a: " ++) $ show $ product [1..12] + (98 * 86)
