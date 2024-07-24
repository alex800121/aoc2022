{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Day21 (day21) where

import Control.Applicative ((<|>))
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace
import MyLib
import Paths_AOC2022
import Text.Megaparsec
import Text.Megaparsec.Char

data Op = Plus | Minus | Multiply | Divide deriving (Show, Eq, Ord)

data Monkey a = Yell a | Calc Op (Monkey a) (Monkey a) deriving (Show, Eq, Ord)

type MonkeyName a = Map String (Monkey a)

newtype Alg a = Alg [a] deriving (Show, Eq)

instance (Num a) => Num (Alg a) where
  Alg a + Alg b = Alg (zipWith' (+) a b)
  Alg a - Alg b = Alg (zipWith' (-) a b)
  Alg a * Alg b = Alg (mul a b)
    where
      mul a b = foldl1' (zipWith' (+)) $ zipWith (\x -> map (* x)) a $ [x ++ b | x <- map (`replicate` 0) [0 ..]]
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

zipWith' _ [] xs = xs
zipWith' _ xs [] = xs
zipWith' g (x : xs) (y : ys) = g x y : zipWith' g xs ys

reduceMonkeyTree :: Monkey (Alg Integer) -> Monkey (Alg Integer)
reduceMonkeyTree m = let m' = f m in if m == m' then m else reduceMonkeyTree m'
  where
    f (Yell a) = Yell a
    f (Calc Divide a b) = case (reduceMonkeyTree a, reduceMonkeyTree b) of
      (Yell (Alg [a']), Yell (Alg [b'])) -> Yell $ Alg [a' `div` b']
      (a', b') -> Calc Divide a' b'
    f (Calc op a b) = case (reduceMonkeyTree a, reduceMonkeyTree b) of
      (Yell a', Yell b') -> Yell $ interpretOp' op a' b'
      (a', b') -> Calc op a' b'

monkeyParser :: (String -> Parser a) -> Parser (Map String (Either a (String, Op, String)))
monkeyParser f =
  (eof >> pure Map.empty) <|> do
    name <- some (anySingleBut ':') <* char ':' <* space
    n <-
      (Left <$> f name <* space) <|> do
        a <- some (satisfy (not . isSpace)) <* space
        b <- (\case '+' -> Plus; '-' -> Minus; '*' -> Multiply; '/' -> Divide) <$> anySingle <* space
        c <- some (satisfy (not . isSpace)) <* space
        pure $ Right (a, b, c)
    Map.insert name n <$> monkeyParser f

buildMonkeyTree :: Map String (Either a (String, Op, String)) -> String -> Monkey a
buildMonkeyTree m s = case m Map.! s of
  Left i -> Yell i
  Right (a, b, c) -> Calc b (buildMonkeyTree m a) (buildMonkeyTree m c)

calcMonkeyTree :: (Op -> a -> a -> a) -> Monkey a -> a
calcMonkeyTree f (Yell a) = a
calcMonkeyTree f (Calc a b c) = f a (calcMonkeyTree f b) (calcMonkeyTree f c)

interpretOp :: (Integral a) => Op -> a -> a -> a
interpretOp Plus = (+)
interpretOp Minus = (-)
interpretOp Multiply = (*)
interpretOp Divide = div

interpretOp' :: (Num a) => Op -> a -> a -> a
interpretOp' Plus = (+)
interpretOp' Minus = (-)
interpretOp' Multiply = (*)

day21bParser :: String -> Parser (Alg Integer)
day21bParser "humn" = signedInteger >> pure (Alg [0, 1])
day21bParser _ = signedInteger >>= \x -> pure (Alg [fromIntegral x])

instance (Show a) => Show (Monkey (Alg a) -> Monkey (Alg a)) where
  show f = show (f (Yell (Alg [])))

solveEqual :: (Monkey (Alg Integer), Monkey (Alg Integer)) -> (Alg Integer, Alg Integer)
-- solveEqual (a, b) | trace (show a ++ "\n-----------\n" ++ show b ++ "\n===========\n") False = undefined
solveEqual (Yell a, Yell b) = (a, b)
solveEqual (Yell d, Calc a b c) = solveEqual (Calc a b c, Yell d)
solveEqual (Calc Multiply a b, x) = case reduceMonkeyTree $ Calc Divide x b of
  Yell x' -> solveEqual (reduceMonkeyTree a, reduceMonkeyTree (Yell x'))
  y' -> solveEqual (reduceMonkeyTree b, reduceMonkeyTree $ Calc Divide x a)
solveEqual (Calc Plus a b, x) = case reduceMonkeyTree $ Calc Minus x b of
  Yell x' -> solveEqual (reduceMonkeyTree a, reduceMonkeyTree (Yell x'))
  y' -> solveEqual (reduceMonkeyTree b, reduceMonkeyTree $ Calc Minus x a)
solveEqual (Calc Minus a b, x) = case reduceMonkeyTree $ Calc Plus x b of
  Yell x' -> solveEqual (reduceMonkeyTree a, reduceMonkeyTree (Yell x'))
  y' -> solveEqual (reduceMonkeyTree b, reduceMonkeyTree $ Calc Minus a x)
solveEqual (Calc Divide a b, x) = solveEqual (reduceMonkeyTree a, reduceMonkeyTree $ Calc Multiply x b)

day21 :: IO ()
day21 = do
  x <- getDataDir >>= readFile . (++ "/input/input21.txt")
  let input = fromJust $ parseMaybe (monkeyParser (const (fromIntegral <$> signedInteger))) x
      input' = fromJust $ parseMaybe (monkeyParser day21bParser) x
      Calc _ l r = buildMonkeyTree input' "root"
  putStrLn $ ("day21a: " ++) $ show $ calcMonkeyTree interpretOp $ buildMonkeyTree input "root"
  putStrLn $ ("day21b: " ++) $ show $ (\(x : y : _) -> negate x `div` y) $ (\(Alg x, Alg y) -> zipWith' (-) x y) $ solveEqual (reduceMonkeyTree l, reduceMonkeyTree r)
