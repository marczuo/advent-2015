module Advent2015.Days.Day06 ( part1, part2 ) where

import Control.Monad
import Control.Arrow
import Data.List
import Data.Array
import Data.Char
import Data.Either.Unwrap
import Text.Parsec (parse)
import qualified Text.Parsec as Parsec

-- Data structure

type Coord = (Int, Int)
data Rect = Rect Coord Coord deriving Show
data Instruction = TurnOn Rect | Toggle Rect | TurnOff Rect deriving Show

makeRect :: Int -> Int -> Int -> Int -> Rect
makeRect x1 y1 x2 y2 = Rect (x1,y1) (x2,y2)

-- Logic

gridMin, gridMax :: Coord
gridMin = (0,0); gridMax = (999,999)
initial1 = array (gridMin, gridMax) (zip (points $ Rect gridMin gridMax) (repeat False))
initial2 = array (gridMin, gridMax) (zip (points $ Rect gridMin gridMax) (repeat 0))

points :: Rect -> [Coord]
points (Rect (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

followOneInst1 :: Array Coord Bool -> Instruction -> Array Coord Bool
followOneInst1 arr (TurnOn rect) = arr // [(p,True) | p <- points rect]
followOneInst1 arr (TurnOff rect) = arr // [(p,False) | p <- points rect]
followOneInst1 arr (Toggle rect) = arr // [(p,not (arr ! p)) | p <- points rect]

followOneInst2 :: Array Coord Int -> Instruction -> Array Coord Int
followOneInst2 arr (TurnOn rect) = arr // [(p,(arr ! p)+1) | p <- points rect]
followOneInst2 arr (TurnOff rect) = arr // [(p,maximum [0,(arr ! p)-1]) | p <- points rect]
followOneInst2 arr (Toggle rect) = arr // [(p,(arr ! p)+2) | p <- points rect]

-- Input parsing

numPairParser :: Parsec.Parsec String () (Int,Int)
numPairParser = do
    (xStr, yStr) <- (,) <$> Parsec.many1 Parsec.digit <*>
                            (Parsec.char ',' *> Parsec.many1 Parsec.digit)
    return (read xStr, read yStr)

lineParser :: Parsec.Parsec String () Instruction
lineParser = let tryString = Parsec.try . Parsec.string in do
                 prefix <- Parsec.choice $ map tryString ["turn on ", "toggle ", "turn off "]
                 (x1, y1) <- numPairParser
                 Parsec.string " through "
                 (x2, y2) <- numPairParser
                 return $ case prefix of
                            "turn on " -> TurnOn $ makeRect x1 y1 x2 y2
                            "toggle " -> Toggle $ makeRect x1 y1 x2 y2
                            "turn off " -> TurnOff $ makeRect x1 y1 x2 y2

fileParser :: Parsec.Parsec String () [Instruction]
fileParser = Parsec.endBy lineParser $ Parsec.char '\n'

parseContent content = fromRight $ parse fileParser "" content
part1 = length . filter (==True) . elems . foldl followOneInst1 initial1 . parseContent
part2 = sum . elems . foldl followOneInst2 initial2 . parseContent
