import Control.Monad
import Data.List
import Data.Bits
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P
import Local.IO.AdventOfCode
import Local.Data.Either

today = "7"

-- Data Structure

type UnaryOperation = Int -> Int 
type BinaryOperation = Int -> Int -> Int
type Circuit = Map String Int

data Instruction = Assignment String Int
                 | Unary UnaryOperation String String
                 | Binary BinaryOperation String String String

-- Logic & Input parsing

anyNumber, anyLower, anyUpper :: Parsec String () String
assignmentLiteral, assignment, unary, binary :: Parsec String () Instruction
binaryRightLiteral, binaryLeftLiteral :: Parsec String () Instruction

anyNumber = P.many1 P.digit
anyLower = P.many1 P.lower
anyUpper = P.many1 P.upper

assignment = do
    varRight <- anyLower; P.string " -> "; varLabel <- anyLower
    return $ Unary id varLabel varRight

assignmentLiteral = do
    valueStr <- anyNumber; P.string " -> "; varLabel <- anyLower
    return $ Assignment varLabel (read valueStr) 

unary = do
    opStr <- anyUpper; P.spaces; varRight <- anyLower
    P.string " -> "; varLabel <- anyLower
    return $ Unary complement varLabel varRight

binary = do
    varRight1 <- anyLower; P.spaces; opStr <- anyUpper; P.spaces; varRight2 <- anyLower;
    P.string " -> "; varLabel <- anyLower
    return $ case opStr of
               "LSHIFT" -> Binary shift varLabel varRight1 varRight2
               "RSHIFT" -> Binary (\x y -> x `shift` (-y)) varLabel varRight1 varRight2
               "AND" -> Binary (.&.) varLabel varRight1 varRight2
               "OR"  -> Binary (.|.) varLabel varRight1 varRight2

binaryRightLiteral = do
    varRight <- anyLower; P.spaces; opStr <- anyUpper; P.spaces; numRight <- anyNumber
    P.string " -> "; varLabel <- anyLower
    return $ case opStr of
               "LSHIFT" -> Unary (`shift` read numRight) varLabel varRight
               "RSHIFT" -> Unary (`shift` (-(read numRight))) varLabel varRight 
               "AND" -> Unary (.&. read numRight) varLabel varRight
               "OR" -> Unary (.|. read numRight) varLabel varRight 

binaryLeftLiteral = do
    numRight <- anyNumber; P.spaces; opStr <- anyUpper; P.spaces; varRight <- anyLower
    P.string " -> "; varLabel <- anyLower
    return $ case opStr of
               "LSHIFT" -> Unary (shift (read numRight)) varLabel varRight
               "RSHIFT" -> Unary (shift (-(read numRight))) varLabel varRight 
               "AND" -> Unary (read numRight .&.) varLabel varRight
               "OR" -> Unary (read numRight .|.) varLabel varRight 

lineParser :: Parsec String () Instruction
lineParser = P.choice $ map P.try [assignmentLiteral, assignment, unary,
                                            binary, binaryRightLiteral, binaryLeftLiteral]

-- Credits to this post:
--   https://www.reddit.com/r/adventofcode/comments/3vvbtw/day_7_haskell_help_with_trying_to_lookup_a_map/

makeCircuit :: [Instruction] -> Circuit
makeCircuit instructions = circuit where
    circuit = Map.fromList $ map connect instructions
    connect (Assignment var value) = (var, value)
    connect (Unary op var varRight) = (var, op (get varRight))
    connect (Binary op var varRight1 varRight2) = (var, op (get varRight1) (get varRight2))
    get var = circuit ! var

parseFile :: String -> [Instruction]
parseFile content = errorOnLeft $ parse (P.endBy lineParser $ P.char '\n') content content

main :: IO ()
main = adventIO today parseContent part1 part2 where
    argsToFileName = head
    parseContent content = let insts = parseFile content in (makeCircuit insts,insts)
    part1 (circuit,insts) = circuit ! "a"
    part2 (circuit,insts) = makeCircuit (insts ++ [Assignment "b" (circuit ! "a")]) ! "a"
