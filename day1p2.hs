import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

instsToFloors :: String -> [Int]
instsToFloors = scanl (flip step) 0 where
    step '(' = succ
    step ')' = pred

firstBasement :: String -> Int
firstBasement = length . takeWhile (>=0) . instsToFloors

main :: IO ()
main = readApplyPrint argsToFileName parseContent firstBasement where
    argsToFileName = head
    parseContent = head . lines
