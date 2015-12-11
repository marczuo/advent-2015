import Control.Monad
import Control.Arrow
import Data.List
import Local.IO.AdventOfCode

today = "3"

type Coord = (Int,Int)

instsToCoords :: String -> [Coord]
instsToCoords = scanl oneMove (0,0) where
    oneMove (x,y) op | op == '<' = (pred x,y)
                     | op == '>' = (succ x,y)
                     | op == '^' = (x,pred y)
                     | op == 'v' = (x,succ y)

splitInsts :: String -> (String, String)
splitInsts "" = ("", "")
splitInsts (a:b:xs) = (a:oldA, b:oldB) where (oldA, oldB) = splitInsts xs
splitInsts (a:xs) = (a:oldA, oldB) where (oldA, oldB) = splitInsts xs

instsToCoords2 :: String -> [Coord]
instsToCoords2 = uncurry (++) . join (***) instsToCoords . splitInsts

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = head . lines
    part1 = length . nub . instsToCoords
    part2 = length . nub . instsToCoords2
