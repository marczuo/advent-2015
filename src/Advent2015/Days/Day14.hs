module Advent2015.Days.Day14 ( part1, part2 ) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Data.Either.Unwrap
import Text.Parsec (parse, Parsec)
import qualified Text.Parsec as P

-- Data structure

type Reindeer = (Int, Int, Int)

-- Logic

maxi :: Ord a => [a] -> Int
maxi xs = snd $ maximumBy (comparing fst) (zip xs [0..])

kronecker :: Int -> [Int]
kronecker x = map (\i -> if i==x then 1 else 0) [0..]

simulateTo :: Int -> [Reindeer] -> [Int]
simulateTo sec = map (runReindeer sec) where
    runReindeer sec (spd, dur, rst) = let quot = sec `div` (dur+rst)
                                          rem  = sec `mod` (dur+rst) in
                                          spd*minimum [rem,dur] + spd*dur*quot

assignPt :: Int -> [Reindeer] -> [Int]
assignPt sec deers | sec == 0  = replicate (length deers) 0
                   | otherwise = let winning = maximum distances
                                     distances = simulateTo sec deers in
                                     zipWith (+) (map (\i -> if (distances !! i)==winning
                                                             then 1 else 0) [0..])
                                                 (assignPt (sec-1) deers)

-- Input parsing

lineParser :: Parsec String () Reindeer
fileParser :: Parsec String () [Reindeer]

lineParser = let number = read <$> P.many (P.oneOf ('-':['0'..'9'])) in
                 (,,) <$ P.manyTill P.letter (P.char ' ')
                      <* P.string "can fly " <*> number
                      <* P.string " km/s for " <*> number
                      <* P.string " seconds, but then must rest for " <*> number
                      <* P.string " seconds."

fileParser = P.endBy lineParser $ P.char '\n'

parseContent content = fromRight $ parse fileParser content content
part1 = maximum . simulateTo 2503 . parseContent
part2 = maximum . assignPt 2503 . parseContent
