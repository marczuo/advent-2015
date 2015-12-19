module Advent2015.Days.Day17 ( part1, part2 ) where

import Data.List
import Data.Function

initialRun :: [[Int]] -> [[Int]]
initialRun [] = []
initialRun [x] = [x]
initialRun (x:y:xs) = if length x == length y then x:initialRun (y:xs) else [x]

parseContent :: String -> [Int]
part1, part2 :: String -> Int
parseContent = map read . lines
part1 = length . filter ((==150) . sum) . subsequences . parseContent
part2 = length . initialRun . sortBy (compare `on` length) . filter ((==150) . sum) .
        subsequences . parseContent
