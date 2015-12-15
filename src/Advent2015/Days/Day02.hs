module Advent2015.Days.Day02 ( part1, part2 ) where

import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Advent2015.Data.List

surfaces, perimeters :: [Int] -> [Int] 

strToDimensions :: String -> [Int]
getRibbonLength1 :: [Int] -> Int
getRibbonLength2 :: [Int] -> Int

surfaces = map (uncurry (*)) . combinationNoDiag
perimeters = map ((2*) . uncurry (+)) . combinationNoDiag

strToDimensions = map read . splitOn "x"
getRibbonLength1 = ((+) <$> ((*2) . sum) <*> minimum) . surfaces
getRibbonLength2 = (+) <$> product <*> minimum . perimeters

part1 = sum . map (getRibbonLength1 . strToDimensions) . lines
part2 = sum . map (getRibbonLength2 . strToDimensions) . lines
