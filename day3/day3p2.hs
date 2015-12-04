import System.Environment
import System.IO
import Control.Monad
import Control.Arrow
import Data.List

data Coord = Coord Int Int deriving (Show, Eq, Ord)
xOf, yOf :: Coord -> Int
xOf (Coord x _) = x 
yOf (Coord _ y) = y

instsToCoords :: String -> [Coord]
instsToCoords = scanl oneMove (Coord 0 0) where
    oneMove c '<' = Coord ((xOf c)-1) (yOf c)
    oneMove c '>' = Coord ((xOf c)+1) (yOf c)
    oneMove c '^' = Coord (xOf c) ((yOf c)+1)
    oneMove c 'v' = Coord (xOf c) ((yOf c)-1)

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

    applyAndPrintResult :: Show b => (a -> b) -> a -> IO ()
    applyAndPrintResult function = print . function 

    readApplyPrint :: Show b => ([String] -> String) -> (String -> a) -> (a -> b) -> IO ()
    readApplyPrint argsToFileName parseContent function = getArgs >>= readFile . argsToFileName
                                                                  >>= applyAndPrintResult function . parseContent
