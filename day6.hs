import System.Environment
import System.IO
import Control.Monad
import Control.Arrow
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Text.Parsec (parse)
import qualified Text.Parsec as Parsec
import IO.ReadApplyPrint

data Instruction = TurnOn Int Int Int Int
                 | Toggle Int Int Int Int
                 | TurnOff Int Int Int Int
     deriving (Show)

type Light = (Int,Int)

-- These are for convenience
(/\), (\/), (\\\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection; (\/) = Set.union; (\\\) = Set.difference

-- Logic

testInst = TurnOn 10 20 15 25
testInst2 = TurnOff 11 21 14 24

followOneInst :: Set Light -> Instruction -> Set Light
followOneInst prev inst = 
    let genRange x1 y1 x2 y2 = Set.fromList [(x,y) | x <- [x1..x2], y <- [y1..y2]]
        (\\//) prev togg = prev \/ togg \\\ (togg /\ prev) in
        case inst of
          TurnOn x1 y1 x2 y2 -> prev \/ genRange x1 y1 x2 y2
          Toggle x1 y1 x2 y2 -> prev \\// genRange x1 y1 x2 y2
          TurnOff x1 y1 x2 y2 -> prev \\\ genRange x1 y1 x2 y2

followListInsts :: [Instruction] -> Set Light
followListInsts = foldl followOneInst Set.empty

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
                            "turn on " -> TurnOn x1 y1 x2 y2
                            "toggle " -> Toggle x1 y1 x2 y2
                            "turn off " -> TurnOff x1 y1 x2 y2

fileParser :: Parsec.Parsec String () [Instruction]
fileParser = Parsec.endBy lineParser $ Parsec.char '\n'

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    -- Not handling error here since input is assumed to be well-formed
    parseContent content = let (Right result) = parse fileParser "" content in result
    findAnswer = Set.size . followListInsts
