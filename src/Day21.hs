module Day21 (day21) where

import MyLib
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (fromJust)
import Data.List
import Data.Foldable

initPW :: Seq Char
initPW = Seq.fromList "abcdefgh"

scrambledPW :: Seq Char
scrambledPW = Seq.fromList "fbgdceah"

data Ins = SwapPos Int Int
         | SwapLet Char Char
         | Rotate Int
         | RotateOn Char
         | RevPos Int Int
         | MovePos Int Int
  deriving (Show, Eq, Ord)

rotateL :: Seq a -> Seq a
rotateL (x :<| xs) = xs :|> x

rotateR :: Seq a -> Seq a
rotateR (xs :|> x) = x :<| xs

readIns :: Seq Char -> Ins -> Seq Char
readIns s (SwapPos x y) = Seq.adjust' (const (Seq.index s y)) x
                        $ Seq.adjust' (const (Seq.index s x)) y s
readIns s (SwapLet a b) = Seq.adjust' (const b) x
                        $ Seq.adjust' (const a) y s
  where
    Just x = Seq.elemIndexL a s
    Just y = Seq.elemIndexL b s
readIns s (Rotate n)
  | n >= 0 = iterate rotateR s !! n
  | otherwise = iterate rotateL s !! negate n
readIns s (RotateOn c) = iterate rotateR s !! (x + 1 + if x >= 4 then 1 else 0)
  where
    Just x = Seq.elemIndexL c s
readIns s (RevPos x y) = Seq.take x s <> Seq.reverse (Seq.take (y - x + 1) $ Seq.drop x s) <> Seq.drop (y + 1) s
readIns s (MovePos x y) = Seq.insertAt y a $ Seq.deleteAt x s
  where
    a = Seq.index s x

unscramble :: Seq Char -> Ins -> Seq Char
unscramble s (SwapPos x y) = Seq.adjust' (const (Seq.index s y)) x
                           $ Seq.adjust' (const (Seq.index s x)) y s
unscramble s (SwapLet a b) = Seq.adjust' (const b) x
                           $ Seq.adjust' (const a) y s
  where
    Just x = Seq.elemIndexL a s
    Just y = Seq.elemIndexL b s
unscramble s (Rotate n)
  | n >= 0 = iterate rotateL s !! n
  | otherwise = iterate rotateR s !! negate n
unscramble s (RevPos x y) = Seq.take x s <> Seq.reverse (Seq.take (y - x + 1) $ Seq.drop x s) <> Seq.drop (y + 1) s
unscramble s (MovePos y x) = Seq.insertAt y a $ Seq.deleteAt x s
  where
    a = Seq.index s x
unscramble s (RotateOn c)
  | odd x = iterate rotateL s !! ((x + 1) `div` 2)
  | otherwise = iterate rotateL s !! (((((x - 1) `mod` 8) + 1) `div` 2) + 5)
  where
    Just x = Seq.elemIndexL c s
-- unscramble s (RotateOn c) = iterate rotateR s !! (x + 1 + if x >= 4 then 1 else 0)
--   where
--     Just x = Seq.elemIndexL c s

insParser :: Parser Ins
insParser =
      ( MovePos <$> (string "move position " >> signedInteger) <*> (string " to position " >> signedInteger) )
  <|> ( SwapPos <$> (string "swap position " >> signedInteger) <*> (string " with position " >> signedInteger) )
  <|> ( RevPos <$> (string "reverse positions " >> signedInteger) <*> (string " through " >> signedInteger) )
  <|> ( Rotate <$> (string "rotate right " >> signedInteger <* string " step" <* optional (char 's')) )
  <|> ( Rotate <$> (string "rotate left " >> (negate <$> signedInteger) <* string " step" <* optional (char 's')) )
  <|> ( RotateOn <$> (string "rotate based on position of letter " >> anySingle) )
  <|> ( SwapLet <$> (string "swap letter " >> anySingle) <*> (string " with letter " >> anySingle) )

day21 :: IO ()
day21 = do
  ins <- map (fromJust . parseMaybe insParser) . lines <$> readFile "input21.txt"
  putStrLn $ ("day21a: " ++) $ toList $ foldl' readIns initPW ins
  putStrLn $ ("day21b: " ++) $ toList $ foldl' unscramble scrambledPW $ reverse ins