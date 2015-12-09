import System.Environment
import System.IO
import Control.Monad
import Local.IO.AdventOfCode

today = "1" 

step :: Char -> (Int -> Int)
instsToFloor :: String -> Int
instsToFloorsList :: String -> [Int]

step '(' = succ
step ')' = pred 

instsToFloor = foldl (flip step) 0
instsToFloorsList = scanl (flip step) 0

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = head . lines
    part1 = instsToFloor
    part2 = length . takeWhile (>=0) . instsToFloorsList
