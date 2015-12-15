module Advent2015.Days.Day04 ( parseContent, part1, part2 ) where

import Data.List
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import qualified Data.Digest.Pure.MD5 as MD5
import Advent2015.Data.List

today = "4"

md5AsString :: String -> String
md5StartsWithNZeros :: Int -> String -> Bool
findAnswerN :: Int -> String -> Int

md5AsString = show . MD5.md5 . BSChar8.pack 
md5StartsWithNZeros = (. md5AsString) . isPrefixOf . flip replicate '0'
findAnswerN n prefix = length . takeWhile (==False) $
    map (md5StartsWithNZeros n . (prefix++) . show) [0..]

parseContent :: String -> String
part1, part2 :: String -> Int

parseContent = head . lines
part1 = findAnswerN 5
part2 = findAnswerN 6
