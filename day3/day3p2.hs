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
instsToCoords2 = uncurry (++) . join (***) instsToCoords . splitInsts

instsToCount2 :: String -> Int
instsToCount2 = length . nub . instsToCoords2

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = head $ lines content in
              print $ instsToCount2 input
