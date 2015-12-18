-- Advent of code 2015

import qualified Advent2015.Days.Day01 as Day01
import qualified Advent2015.Days.Day02 as Day02
import qualified Advent2015.Days.Day03 as Day03
import qualified Advent2015.Days.Day04 as Day04
import qualified Advent2015.Days.Day05 as Day05
import qualified Advent2015.Days.Day06 as Day06
import qualified Advent2015.Days.Day07 as Day07
import qualified Advent2015.Days.Day08 as Day08
import qualified Advent2015.Days.Day09 as Day09
import qualified Advent2015.Days.Day10 as Day10
import qualified Advent2015.Days.Day11 as Day11
import qualified Advent2015.Days.Day12 as Day12
import qualified Advent2015.Days.Day13 as Day13
import qualified Advent2015.Days.Day14 as Day14
import qualified Advent2015.Days.Day15 as Day15
import qualified Advent2015.Days.Day18 as Day18

import Control.Monad
import Control.Lens
import Data.Maybe
import Advent2015.IO

main :: IO ()
main = do
    (operation, day, input) <- adventIO 
    if operation == noopPrim then return ()
    else do
        if operation /= onlyPart2Prim then do
            putStr $ "day" ++ show day ++ "p1: "
            let p1 = ["", show $ Day01.part1 input, show $ Day02.part1 input,
                          show $ Day03.part1 input, show $ Day04.part1 input,
                          show $ Day05.part1 input, show $ Day06.part1 input,
                          show $ Day07.part1 input, show $ Day08.part1 input,
                          show $ Day09.part1 input, show $ Day10.part1,
                          Day11.part1 input,        show $ Day12.part1 input,
                          show $ Day13.part1 input, show $ Day14.part1 input,
                          show $ Day15.part1 input, "",
                          "",                       show $ Day18.part1 input
                     ] ^? element day in
                putStr $ fromMaybe "unimplemented" p1
            putChar '\n'
        else return ()
        if operation /= onlyPart1Prim then do
            putStr $ "day" ++ show day ++ "p2: "
            let p2 = ["", show $ Day01.part2 input, show $ Day02.part2 input,
                          show $ Day03.part2 input, show $ Day04.part2 input,
                          show $ Day05.part2 input, show $ Day06.part2 input,
                          show $ Day07.part2 input, show $ Day08.part2 input,
                          show $ Day09.part2 input, show $ Day10.part2,
                          Day11.part2 input,        show $ Day12.part2 input,
                          show $ Day13.part2 input, show $ Day14.part2 input,
                          show $ Day15.part2 input, "",
                          "",                       show $ Day18.part2 input
                     ] ^? element day in
                putStr $ fromMaybe "unimplemented" p2
            putChar '\n'
        else return ()
