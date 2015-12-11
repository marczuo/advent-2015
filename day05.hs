import Control.Monad
import Data.List
import Local.IO.AdventOfCode

today = "5"

isNice1 :: String -> Bool
isNice1 str = threeVowels && runOfTwo && noNaughtySubstrings where
    threeVowels = length (filter (`elem` "aeiou") str) >= 3
    runOfTwo = any (uncurry (==)) ((zip <*> tail) str)
    noNaughtySubstrings = all (not . (`isInfixOf` str)) ["ab", "cd", "pq", "xy"]

isNice2 :: String -> Bool
isNice2 = (&&) <$> hasPair <*> hasRepeat where 
    hasPair [] = False; hasPair [x] = False; hasPair [x,y] = False
    hasPair (x:y:xs) = isInfixOf [x,y] xs || hasPair (y:xs)
    hasRepeat = any (uncurry (==)) . (zip <*> (tail . tail))

main :: IO ()
main = adventIO today parseContent part1 part2 where
    argsToFileName = head
    parseContent = lines
    part1 = length . filter isNice1
    part2 = length . filter isNice2
