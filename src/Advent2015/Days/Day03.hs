module Advent2015.Days.Day03 ( parseContent, part1, part2 ) where

import Control.Monad
import Control.Arrow
import Data.List

type Coord = (Int,Int)

instsToCoords :: String -> [Coord]
splitInsts :: String -> (String, String)
instsToCoords2 :: String -> [Coord]

instsToCoords = scanl oneMove (0,0) where
    oneMove (x,y) op | op == '<' = (pred x,y)
                     | op == '>' = (succ x,y)
                     | op == '^' = (x,pred y)
                     | op == 'v' = (x,succ y)

splitInsts "" = ("", "")
splitInsts (a:b:xs) = (a:oldA, b:oldB) where (oldA, oldB) = splitInsts xs
splitInsts (a:xs) = (a:oldA, oldB) where (oldA, oldB) = splitInsts xs

instsToCoords2 = uncurry (++) . join (***) instsToCoords . splitInsts

parseContent = head . lines
part1 = length . nub . instsToCoords . parseContent
part2 = length . nub . instsToCoords2 . parseContent
