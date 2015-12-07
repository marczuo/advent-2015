import System.Environment
import System.IO
import Control.Monad
import Control.Arrow
import Data.List
import Data.Array
import Data.Char
import Text.Parsec (parse)
import qualified Text.Parsec as Parsec
import IO.ReadApplyPrint

type Coord = (Int, Int)
data Rect = Rect Coord Coord deriving Show
data Instruction = TurnOn Rect | Toggle Rect | TurnOff Rect deriving Show

makeRect :: Int -> Int -> Int -> Int -> Rect
makeRect x1 y1 x2 y2 = Rect (x1,y1) (x2,y2)

-- Logic

gridMin = (0,0); gridMax = (999,999)
initial = array (gridMin, gridMax) (zip (points $ Rect gridMin gridMax) (repeat 0))

points :: Rect -> [Coord]
points (Rect (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

followOneInst :: Array Coord Int -> Instruction -> Array Coord Int
followOneInst arr (TurnOn rect) = arr // [(p,(arr ! p)+1) | p <- points rect]
followOneInst arr (TurnOff rect) = arr // [(p,maximum [0,(arr ! p)-1]) | p <- points rect]
followOneInst arr (Toggle rect) = arr // [(p,(arr ! p)+2) | p <- points rect]

followListInsts :: [Instruction] -> Array Coord Int
followListInsts = foldl' followOneInst initial

sumArray :: Array Coord Int -> Int
sumArray = sum . elems

-- Input parsing

numPairParser :: Parsec.Parsec String () (Int,Int)
numPairParser = do
    (xStr, yStr) <- (,) <$> (Parsec.many1 Parsec.digit) <*> (Parsec.char ',' *> Parsec.many1 Parsec.digit)
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

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    -- Not handling error here since input is assumed to be well-formed
    parseContent content = let (Right result) = parse fileParser "" content in result
    findAnswer = sumArray . followListInsts
