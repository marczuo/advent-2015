import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

isNice :: String -> Bool
isNice = (&&) <$> hasPair <*> hasRepeat where 

    hasPair :: Eq a => [a] -> Bool
    hasPair [] = False; hasPair [x] = False; hasPair [x,y] = False
    hasPair (x:xs) = isInfixOf [x, head xs] (tail xs) || hasPair xs

    hasRepeat :: Eq a => [a] -> Bool
    hasRepeat str = foldr (||) False $ map (uncurry (==)) $ (zip <*> (tail . tail)) str

countNice :: [String] -> Int
countNice = length . filter isNice

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = countNice
