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

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = head $ lines content in
              print $ instsToCount input
