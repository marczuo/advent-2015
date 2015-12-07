import System.Environment
import System.IO
import Control.Monad
import Control.Arrow
import Data.List
import IO.ReadApplyPrint

type Coord = (Int,Int)

instsToCoords :: String -> [Coord]
instsToCoords = scanl (flip oneMove) (0, 0) where
    oneMove :: Char -> Coord -> Coord
    oneMove '<' (x,y) = ((x-1), y)
    oneMove '>' (x,y) = ((x+1), y)
    oneMove '^' (x,y) = (x, (y+1))
    oneMove 'v' (x,y) = (x, (y-1))

splitInsts :: String -> (String, String)
splitInsts "" = ("", "")
splitInsts (a:b:xs) = (a:oldA, b:oldB) where
    (oldA, oldB) = splitInsts xs
splitInsts (a:xs) = (a:oldA, oldB) where
    (oldA, oldB) = splitInsts xs

instsToCoords2 :: String -> [Coord]
instsToCoords2 = uncurry (++) . mapToPair instsToCoords . splitInsts where
    mapToPair = join (***)

instsToCount2 :: String -> Int
instsToCount2 = length . nub . instsToCoords2

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = head . lines
    findAnswer = instsToCount2
