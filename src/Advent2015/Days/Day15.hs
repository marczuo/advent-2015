module Advent2015.Days.Day15 ( part1, part2 ) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Either.Unwrap
import Text.Parsec (parse, Parsec)
import qualified Text.Parsec as P

today = "15"

-- Data structure

type Ingredient = (Int, Int, Int, Int, Int)

-- Logic

calcValue :: [Ingredient] -> [Int] -> Int
calcValue ing cnt = let ling = map (\(a,b,c,d,e) -> [a,b,c,d,e]) ing
                        nullify n = maximum [n, 0] in 
                        product $ map (nullify . sum) $ init $ transpose $ zipWith (map . (*)) cnt ling 

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf n l | n == 0    = []
               | n == 1    = map return l
               | otherwise = concatMap (\sl -> map (\e -> sl ++ [e]) l) (sublistsOf (n-1) l)

combinations :: Int -> [[Int]]
combinations n = filter ((100 ==) . sum) (sublistsOf n [0..100])

isCalorieGood :: [Ingredient] -> [Int] -> Bool
isCalorieGood ing cnt = let ling = map (\(a,b,c,d,e) -> [a,b,c,d,e]) ing
                            cal  = last (transpose ling) in
                            (sum $ zipWith (*) cal cnt) == 500

getBest :: [Ingredient] -> Int
getBest ing = maximum $ map (calcValue ing) (combinations $ length ing)

getBest2 :: [Ingredient] -> Int
getBest2 ing = maximum $ map (calcValue ing) (filter (isCalorieGood ing) (combinations $ length ing))

-- Input parsing

lineParser :: Parsec String () Ingredient
fileParser :: Parsec String () [Ingredient]

lineParser = let number = read <$> P.many (P.oneOf ('-':['0'..'9'])) in
                 (,,,,) <$ P.manyTill P.letter (P.char ':') <* P.spaces
                        <* P.string "capacity " <*> number <* P.string ", " 
                        <* P.string "durability " <*> number <* P.string ", "
                        <* P.string "flavor " <*> number <* P.string ", "
                        <* P.string "texture " <*> number <* P.string ", "
                        <* P.string "calories " <*> number

fileParser = P.endBy lineParser $ P.char '\n'

parseContent content = fromRight $ parse fileParser content content
part1 = getBest . parseContent
part2 = getBest2 . parseContent
