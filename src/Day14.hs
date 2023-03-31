module Day14 (day14) where

import MyLib
import Crypto.Hash.MD5
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.String
import Data.ByteString.Base16
import Data.List
import Data.Maybe

input :: ByteString
input = fromString "cuanljph"

test :: ByteString
test = fromString "abc"

hashListN :: Int -> ByteString -> Int -> ByteString
hashListN n s = (!! n) . iterate (encode . hash) . (s <>) . fromString . show

hashList :: Int -> ByteString -> [ByteString]
hashList n s = map (hashListN n s) [0..]

calcKey :: Int -> Int -> ByteString -> (Maybe Char, [Char], ByteString)
calcKey n1 n2 x = (listToMaybe $ hasNConsecutives n1 x, hasNConsecutives n2 x, x)

isKey :: Int -> [(Maybe Char, [Char], ByteString)] -> Bool
isKey _ ((Nothing, _, _) : _) = False
isKey n ((Just x, _, _) : xs) = any (elem x . snd') $ take 1000 xs

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

hasNConsecutives :: Int -> ByteString -> [Char]
hasNConsecutives n s = map BS.head $ filter ((>= n) . BS.length) $ BS.group s

day14 :: IO ()
day14 = do
  putStrLn $ ("day14a: " ++) $ show $ (!! 63) $ findIndices (isKey 1000) $ tails $ map (calcKey 3 5) $ hashList 1 input
  putStrLn $ ("day14b: " ++) $ show $ (!! 63) $ findIndices (isKey 1000) $ tails $ map (calcKey 3 5) $ hashList 2017 input
