import System.Environment
import System.IO
import Control.Monad
import Data.List

instsToFloors :: String -> [Int]
instsToFloors = scanl (flip step) 0 where
    step '(' = succ
    step ')' = pred

firstBasement :: String -> Int
firstBasement = length . takeWhile (>=0) . instsToFloors

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = (lines content) !! 0 in
              print $ firstBasement input
