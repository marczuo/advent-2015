import System.Environment
import System.IO
import Control.Monad
import Control.Arrow
import Data.List
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as Parsec
import Local.IO.AdventOfCode
import Local.Data.List

today = "9"

-- Data structure

type Graph = [((String,String), Int)]

lookupGraph :: (String,String) -> Graph -> Maybe Int
lookupGraph (start,end) g | start == end = Just 0
lookupGraph (start,end) g | otherwise    = case lookup (start,end) g of
                                             Nothing -> lookup (end,start) g
                                             Just i -> Just i

graphToVertices :: Graph -> [String]
graphToVertices graph = nub $ concat [[x,y] | (x,y) <- toIndexList graph] where
    toIndexList = fst . unzip

negateGraph :: Graph -> Graph
negateGraph = map (\(label,value) -> (label,-value))

-- Logic

minMaybe :: Ord a => [Maybe a] -> Maybe a
minMaybe list = let notNothing Nothing = False
                    notNothing (Just _) = True in 
                    case (filter notNothing list) of 
                      [] -> Nothing
                      nonEmpty -> minimum nonEmpty

solveTSPHelper :: [String] -> (String,String) -> Graph -> Maybe Int
solveTSPHelper s (x,y) g = let betw = s \\ [x,y] in
                               if betw == [] then lookupGraph (x,y) g else minMaybe $
                                   map (\v -> ((+) <$> (solveTSPHelper (delete v betw) (x,v) g)
                                                   <*> (lookupGraph (v,y) g))) betw 

solveTSP :: Graph -> Maybe Int
solveTSP g = minMaybe $ map (\p -> solveTSPHelper vertices p g) vertexPairs where
    vertices = graphToVertices g
    vertexPairs = combinationNoDiag vertices

-- Input parsing

lineParser :: Parsec String () ((String,String),Int)
lineParser = let aWord = Parsec.many1 Parsec.letter
                 aNum = Parsec.many1 Parsec.digit in do
                     start <- aWord; Parsec.string " to "; end <- aWord
                     Parsec.string " = "; dist <- aNum
                     return ((start,end),read dist)

readGraph :: [String] -> Graph
readGraph = map parseLine where
    parseLine line = case parse lineParser line line of
                       Left err -> error $ show err
                       Right result -> result

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = readGraph . lines
    part1 = solveTSP
    part2 = (negate <$>) . solveTSP . map (second negate)
