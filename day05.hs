import Control.Monad
import Data.List
import Local.IO.AdventOfCode

today = "5"

isNice1 :: String -> Bool
isNice1 str = (threeVowels str) && (runOfTwo str) && (noNaughtySubstrings str) where
    threeVowels str = length (filter (`elem` "aeiou") str) >= 3
    runOfTwo str = foldr (||) False $ map (uncurry (==)) $ (zip <*> tail) str
    noNaughtySubstrings str = not $ foldr (||) False $ map (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

isNice2 :: String -> Bool
isNice2 = (&&) <$> hasPair <*> hasRepeat where 
    hasPair [] = False; hasPair [x] = False; hasPair [x,y] = False
    hasPair (x:xs) = isInfixOf [x, head xs] (tail xs) || hasPair xs 
    hasRepeat = foldr (||) False . (map (uncurry (==))) . (zip <*> (tail . tail))

main :: IO ()
main = adventIO today parseContent part1 part2 where
    argsToFileName = head
    parseContent = lines
    part1 = length . filter isNice1
    part2 = length . filter isNice2
