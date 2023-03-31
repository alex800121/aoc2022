{-# LANGUAGE OverloadedStrings #-}
module Day5 (day5) where

import MyLib
-- import Data.Digest.Pure.MD5
import Data.ByteString.Base16
import Crypto.Hash.MD5
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString
import Data.String (fromString)
import Data.List (delete)
import Data.Char (digitToInt)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Debug.Trace

input :: ByteString
input = "cxdnnyjw"
-- input = "abc"

inputList :: [ByteString]
inputList = map ((input <>) . fromString . show)  [0..]

hashList :: [ByteString]
hashList = map (encode . hash) inputList

has5Zero :: [ByteString]
has5Zero = filter ("00000" `ByteString.isPrefixOf`) hashList

take8NonZero :: ByteString
take8NonZero = ByteString.pack $ map (`ByteString.index` 5) $ take 8 has5Zero

buildPW :: ByteString -> String -> [ByteString] -> ByteString
buildPW temp seen (x : xs)
  -- | trace (ByteString.unpack $ temp <> (',' `ByteString.cons` fromString seen)) False = undefined
  | null seen =  temp
  | pos `notElem` seen = buildPW temp seen xs
  | otherwise = buildPW temp' seen' xs
  where
    pos = x `ByteString.index` 5
    pos' = digitToInt pos
    seen' = delete pos seen
    temp' = ByteString.take pos' temp <> ((x `ByteString.index` 6) `ByteString.cons` ByteString.drop (pos' + 1) temp)

day5 :: IO ()
day5 = do
  ByteString.putStr $ fromString "day5a: " <> take8NonZero
  putStrLn ""
  ByteString.putStr $ fromString "day5b: " <> buildPW (fromString "xxxxxxxx") "01234567" has5Zero
  putStrLn ""
