import Control.Monad
import Control.Arrow
import Data.List
import Data.List.Extra
import Data.Maybe
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as Parsec
import Local.IO.AdventOfCode
import Local.Data.List
import Local.Data.Either

today = "9"

-- Data structure

type Graph = [((String,String), Int)]

lookupGraph :: (String,String) -> Graph -> Maybe Int
lookupGraph (start,end) g | start == end = Just 0
lookupGraph (start,end) g | otherwise    = case lookup (start,end) g of
                                             Nothing -> lookup (end,start) g
                                             Just i -> Just i

graphToVertices :: Graph -> [String]
graphToVertices graph = nubOrd $ concat [[x,y] | (x,y) <- toIndexList graph] where 
    toIndexList = fst . unzip

negateGraph :: Graph -> Graph
negateGraph = map (\(label,value) -> (label,-value))

-- Logic

minMaybe :: Ord a => [Maybe a] -> Maybe a
minMaybe list = case (catMaybes list) of 
                  [] -> Nothing
                  nonEmpty -> Just (minimum nonEmpty)

solveTSPHelper :: [String] -> (String,String) -> Graph -> Maybe Int
solveTSPHelper s (x,y) g = let betw = s \\ [x,y] in
                               if betw == [] then lookupGraph (x,y) g else minMaybe $
                                   map (\v -> ((+) <$> (solveTSPHelper (delete v betw) (x,v) g)
                                                   <*> (lookupGraph (v,y) g))) betw 

solveTSP :: Graph -> Int
solveTSP g = let vertices = graphToVertices g
                 vertexPairs = combinationNoDiag vertices
                 solution = minMaybe $ map (\p -> solveTSPHelper vertices p g) vertexPairs in
                 fromMaybe (error "Error: Graph not connected?") solution

-- Input parsing

lineParser :: Parsec String () ((String,String),Int)
lineParser = let aWord = Parsec.many1 Parsec.letter
                 aNum = Parsec.many1 Parsec.digit in do
                     start <- aWord; Parsec.string " to "; end <- aWord
                     Parsec.string " = "; dist <- aNum
                     return ((start,end),read dist)

readGraph :: [String] -> Graph
readGraph = map parseLine where
    parseLine line = errorOnLeft $ parse lineParser line line

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = readGraph . lines
    part1 = solveTSP
    part2 = negate . solveTSP . map (second negate)
