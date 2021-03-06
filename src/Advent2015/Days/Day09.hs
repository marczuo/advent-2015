module Advent2015.Days.Day09 ( part1, part2 ) where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Either.Unwrap
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P
import Advent2015.Data.List
import Advent2015.Data.Graph
import Advent2015.Data.Maybe

-- Logic

solveTSPHelper :: [Int] -> (Int,Int) -> Graph Int -> Maybe Int
solveTSPHelper s (x,y) g = let betw = s \\ [x,y] in
                               if null betw then lookupGraph (x,y) g else minMaybe $
                                   map (\v -> (+) <$> solveTSPHelper (delete v betw) (x,v) g
                                                  <*> lookupGraph (v,y) g) betw 

solveTSP :: Graph Int -> Int
solveTSP g = let vertices = [1..nvertices g]
                 vertexPairs = combinationNoDiag vertices
                 solution = minMaybe $ map (\p -> solveTSPHelper vertices p g) vertexPairs in
                 fromMaybe (error "Error: Graph not connected?") solution

-- Input parsing

lineParser :: Parsec String () (Association2 String Int)
fileParser :: Parsec String () (Graph Int)

lineParser = let aWord = P.many1 P.letter
                 aNum = P.many1 P.digit in do
                     start <- aWord; P.string " to "; end <- aWord
                     P.string " = "; dist <- aNum
                     return ((start,end),read dist)

fileParser = liftA (makeSymmetricGraph 0) $ P.endBy lineParser $ P.char '\n'

parseContent content = fromRight $ parse fileParser content content
part1 = solveTSP . parseContent
part2 = negate . solveTSP . negateGraph . parseContent
