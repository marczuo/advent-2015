import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

type Coord = (Int,Int)

instsToCoords :: String -> [Coord]
instsToCoords = scanr oneMove (0, 0) where
    oneMove :: Char -> Coord -> Coord
    oneMove '<' (x,y) = ((x-1), y)
    oneMove '>' (x,y) = ((x+1), y)
    oneMove '^' (x,y) = (x, (y+1))
    oneMove 'v' (x,y) = (x, (y-1))

instsToCount :: String -> Int
instsToCount = length . nub . instsToCoords

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = head . lines
    findAnswer = instsToCount
