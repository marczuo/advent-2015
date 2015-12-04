import System.Environment
import System.IO
import Control.Monad
import Data.List

data Coord = Coord Int Int deriving (Show, Eq, Ord)
xOf, yOf :: Coord -> Int
xOf (Coord x _) = x 
yOf (Coord _ y) = y

instsToCoords :: String -> [Coord]
instsToCoords = scanr oneMove (Coord 0 0) where
    oneMove :: Char -> Coord -> Coord
    oneMove '<' c = Coord ((xOf c)-1) (yOf c)
    oneMove '>' c = Coord ((xOf c)+1) (yOf c)
    oneMove '^' c = Coord (xOf c) ((yOf c)+1)
    oneMove 'v' c = Coord (xOf c) ((yOf c)-1)

instsToCount :: String -> Int
instsToCount = length . nub . instsToCoords

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = head . lines
    findAnswer = instsToCount

    applyAndPrintResult :: Show b => (a -> b) -> a -> IO ()
    applyAndPrintResult function = print . function 

    readApplyPrint :: Show b => ([String] -> String) -> (String -> a) -> (a -> b) -> IO ()
    readApplyPrint argsToFileName parseContent function = getArgs >>= readFile . argsToFileName
                                                                  >>= applyAndPrintResult function . parseContent
