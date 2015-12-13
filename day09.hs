import Control.Monad
import Control.Arrow
import Data.List
import Data.List.Extra
import Data.Maybe
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P
import Local.IO.AdventOfCode
import Local.Data.List
import Local.Data.Either
import Local.Data.Graph
import Local.Data.Maybe

today = "9"

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

fileParser = do
    pregraph <- P.endBy lineParser $ P.char '\n'
    return $ makeSymmetricGraph 0 pregraph

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent content = errorOnLeft $ parse fileParser content content
    part1 = solveTSP
    part2 = negate . solveTSP . negateGraph
