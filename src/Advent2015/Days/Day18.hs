module Advent2015.Days.Day18 ( part1, part2 ) where

import Data.Maybe
import Data.Matrix
import Data.List

-- Data structure

type Grid = Matrix Bool

makeGrid :: Char -> [String] -> Grid
makeGrid = (fromLists .) . map . map . (==)

getAdjacent :: Int -> Int -> Grid -> [Bool]
getAdjacent x y g = mapMaybe (\(x,y) -> safeGet x y g)
                             ([(x',y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]] \\ [(x,y)])

-- Logic

evolveOne :: Bool -> [Bool] -> Bool
evolveOne cell adj = let alives = length $ filter id adj in
                         if cell then alives==2 || alives==3 else alives==3

isCorner :: Int -> (Int,Int) -> Bool
isCorner size = (`elem` [(x',y') | x' <- [1,size], y' <- [1,size]])

evolveGrid :: (Int -> (Int,Int) -> Bool) -> Grid -> Grid
evolveGrid stuck g = let size = nrows g in
                         matrix size size $
                             \(x,y) -> stuck size (x,y) || evolveOne (g ! (x,y)) (getAdjacent x y g)

evolutions :: (Int -> (Int,Int) -> Bool) -> Grid -> [Grid]
evolutions stuck g = g:evolutions stuck (evolveGrid stuck g)

countOns :: Grid -> Int
countOns = length . filter id . toList

parseContent :: String -> Grid
doWithStuck :: (Int -> (Int,Int) -> Bool) -> String -> Int
part1, part2 :: String -> Int
parseContent = makeGrid '#' . lines
doWithStuck stuck = countOns . (!! 100) . evolutions stuck . parseContent
part1 = doWithStuck (const $ const False)
part2 = doWithStuck isCorner
