import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

isNice :: String -> Bool
isNice = (&&) <$> hasPair <*> hasRepeat

hasPair :: Eq a => [a] -> Bool
hasPair [] = False; hasPair [x] = False; hasPair [x,y] = False
hasPair (x:xs) = isInfixOf [x, head xs] (tail xs) || hasPair xs

hasRepeat :: Eq a => [a] -> Bool
hasRepeat str = foldr (||) False $ map (uncurry (==)) $ (zip <*> (tail . tail)) str

countNice :: [String] -> Int
countNice = length . filter isNice

toVerboseOutput :: [String] -> String
toVerboseOutput = unlines . (map processString) where
    processString str = str ++ " is " ++ (if isNice str then "nice" else "naughty")
                            ++ " : it " ++ (if hasPair str then "has" else "does not have")
                            ++ " a repeated pair, and " ++ (if hasRepeat str then "has" else "does not have")
                            ++ " a split run of two."

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = countNice
