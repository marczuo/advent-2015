import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

isNice :: String -> Bool
isNice str = (threeVowels str) && (runOfTwo str) && (noNaughtySubstrings str) where
    threeVowels str = length (filter (`elem` "aeiou") str) >= 3
    runOfTwo str = foldr (||) False $ map (uncurry (==)) $ (zip <*> tail) str
    noNaughtySubstrings str = not $ foldr (||) False $ map (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

countNice :: [String] -> Int
countNice = length . filter isNice

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = countNice
