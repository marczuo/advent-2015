module Advent2015.Days.Day13 ( part1, part2 ) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Either.Unwrap
import Data.Matrix
import Data.List.Extra (delete, (\\))
import Text.Parsec (parse, Parsec)
import qualified Text.Parsec as P
import Advent2015.Data.Graph
import Advent2015.Data.List
import Advent2015.Data.Maybe

today = "13"

-- Logic

symmetricSum :: Graph Int -> Graph Int
symmetricSum g = g + transpose g

-- Add an element to a graph g, connected to all other elements by constant edge weight n.
addElem :: Int -> Graph Int -> Graph Int
addElem n g = let size = nvertices g + 1 in
                  matrix size size $ \(x,y) -> if x<size && y<size then g ! (x,y) else n

solveTSPHelper :: [Int] -> (Int,Int) -> Graph Int -> Maybe Int
solveTSPHelper s (x,y) g = let betw = s \\ [x,y] in
                               if null betw then lookupGraph (x,y) g else maxMaybe $
                                   map (\v -> (+) <$> solveTSPHelper (delete v betw) (x,v) g
                                                  <*> lookupGraph (v,y) g) betw 

solveTSP :: Graph Int -> Int
solveTSP g = let vertices = [1..nvertices g]
                 vertexPairs = combinationNoDiag vertices
                 solution = maxMaybe $ map (\x -> solveTSPHelper vertices (x,x) g) vertices in
                 fromMaybe (error "Error: Graph not connected?") solution 

-- Input parsing

lineParser :: Parsec String () (Association2 String Int)
fileParser :: Parsec String () (Graph Int)

lineParser = let aWord = P.many1 P.letter
                 aNum  = P.many1 P.digit in do
                 name1 <- aWord; P.string " would "
                 gainLose <- P.choice $ map P.string ["gain", "lose"]; P.char ' '
                 value <- aNum; P.string " happiness units by sitting next to "
                 name2 <- aWord; P.string "."
                 return ((name1,name2), case gainLose of
                                          "gain" -> read value
                                          "lose" -> negate $ read value)

fileParser = liftA (makeGraph 0) $ P.endBy lineParser $ P.char '\n'

parseContent content = fromRight $ parse fileParser content content
part1 = solveTSP . symmetricSum . parseContent
part2 = solveTSP . symmetricSum . addElem 0 . parseContent
