module Day10 (day10) where

import MyLib
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Applicative ((<|>))
import Text.Megaparsec
import Text.Megaparsec.Char (string, space)
import Data.Maybe (fromJust, fromMaybe)
import Data.Either
import Data.Bifunctor
import Data.List (sort, find)

data Bot = Bot { chips :: [Int], lowTo :: Either Int Int, highTo :: Either Int Int }
  deriving (Show, Eq, Ord)
type Output = IntMap [Int]

parseInput :: Parser (IntMap Bot)
parseInput = (space >> eof >> return IM.empty) <|>
  -- bot 88 gives low to bot 51 and high to bot 42
  ( do
      string "bot "
      a <- signedInteger
      low <- string " gives low to " >> ( (string "bot " >> Right <$> signedInteger) <|>
                                          (string "output " >> Left <$> signedInteger)
                                        )
      high <- string " and high to " >> ( (string "bot " >> Right <$> signedInteger) <|>
                                          (string "output " >> Left <$> signedInteger)
                                        ) <* space
      IM.insertWith (\(Bot _ y z) (Bot x' _ _) -> Bot x' y z) a (Bot [] low high) <$> parseInput
  ) <|>
  -- value 67 goes to bot 187
  ( do
      string "value "
      a <- signedInteger
      string " goes to bot "
      b <- signedInteger <* space
      IM.insertWith
        (\(Bot x _ _) (Bot x' y z) -> Bot (x ++ x') y z) b (Bot [a] (Left 0) (Left 0)) <$> parseInput
  )

step :: (Output, IntMap Bot) -> (Output, IntMap Bot)
step (output, imb) = IM.foldlWithKey' f (output, imb) imb
  where
    f :: (Output, IntMap Bot) -> Int -> Bot -> (Output, IntMap Bot)
    f (o, i) key (Bot c l h)
      | length c == 2 = (o', IM.adjust clearBot key i')
      | otherwise = (o, i)
      where
        low = minimum c
        high = maximum c
        to = zip [low, high] [l, h]
        o' = foldr
          ((\(x, y) -> IM.insertWith (++) y [x]) . fmap (fromLeft 0))
          o
          (filter (isLeft . snd) to)
        i' = foldr
          ((\(x, y) -> IM.insertWith g y (Bot [x] (Left 0) (Left 0))) . fmap (fromRight 0))
          i
          (filter (isRight . snd) to)
        g (Bot a _ _) (Bot b u v) = Bot (a ++ b) u v
        clearBot (Bot x y z) = Bot [] y z

day10 ::  IO ()
day10 = do
  input <- fromJust . parseMaybe parseInput <$> readFile "input10.txt"
  let l = iterate step (IM.empty, input)
  putStrLn $ ("day10a: " ++) $ show $
    head $ IM.keys $ head $ dropWhile null $
    map (IM.filter (== [17, 61]) . IM.map (sort . chips) . snd) l
  putStrLn $ ("day10b: " ++) $ show $
    (\x -> product (map (head . (x IM.!)) [0,1,2])) $ fromJust $
    find (\x -> not (any (null . fromMaybe [] . (x IM.!?)) [0,1,2])) $ map fst l
