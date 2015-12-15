module Advent2015.Days.Day01 ( part1, part2 ) where

step :: Char -> Int -> Int
parseContent :: String -> String
part1, part2 :: String -> Int

step '(' = succ; step ')' = pred 
parseContent = head . lines 
part1 = foldl (flip step) 0
part2 = length . takeWhile (>=0) . scanl (flip step) 0
