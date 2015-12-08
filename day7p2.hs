import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.Bits
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as Parsec
import IO.ReadApplyPrint

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

anyNumber = Parsec.many1 Parsec.digit
anyLower = Parsec.many1 Parsec.lower
anyUpper = Parsec.many1 Parsec.upper

assignment = do
    varRight <- anyLower; Parsec.string " -> "; varLabel <- anyLower
    return $ Unary id varLabel varRight

assignmentLiteral = do
    valueStr <- anyNumber; Parsec.string " -> "; varLabel <- anyLower
    return $ Assignment varLabel (read valueStr) 

unary = do
    opStr <- anyUpper; Parsec.spaces; varRight <- anyLower
    Parsec.string " -> "; varLabel <- anyLower
    return $ Unary complement varLabel varRight

binary = do
    varRight1 <- anyLower; Parsec.spaces; opStr <- anyUpper; Parsec.spaces; varRight2 <- anyLower;
    Parsec.string " -> "; varLabel <- anyLower
    return $ case opStr of
               "LSHIFT" -> Binary shift varLabel varRight1 varRight2
               "RSHIFT" -> Binary shift varLabel varRight1 varRight2
               "AND" -> Binary (.&.) varLabel varRight1 varRight2
               "OR"  -> Binary (.|.) varLabel varRight1 varRight2

binaryRightLiteral = do
    varRight <- anyLower; Parsec.spaces; opStr <- anyUpper; Parsec.spaces; numRight <- anyNumber
    Parsec.string " -> "; varLabel <- anyLower
    return $ case opStr of
               "LSHIFT" -> Unary (`shift` (read numRight)) varLabel varRight
               "RSHIFT" -> Unary (`shift` (-(read numRight))) varLabel varRight 
               "AND" -> Unary (.&. (read numRight)) varLabel varRight
               "OR" -> Unary (.|. (read numRight)) varLabel varRight 

binaryLeftLiteral = do
    numRight <- anyNumber; Parsec.spaces; opStr <- anyUpper; Parsec.spaces; varRight <- anyLower
    Parsec.string " -> "; varLabel <- anyLower
    return $ case opStr of
               "LSHIFT" -> Unary (shift (read numRight)) varLabel varRight
               "RSHIFT" -> Unary (shift (-(read numRight))) varLabel varRight 
               "AND" -> Unary ((read numRight) .&.) varLabel varRight
               "OR" -> Unary ((read numRight) .|.) varLabel varRight 

lineParser :: Parsec String () Instruction
lineParser = Parsec.choice $ map Parsec.try [assignmentLiteral, assignment, unary,
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
parseFile content = 
    let result = parse (Parsec.endBy lineParser $ Parsec.char '\n') content content in
        case result of
          Left err -> error $ show err
          Right instructions -> instructions

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent content = let insts = parseFile content in (makeCircuit insts,insts)
    findAnswer (circuit,insts) = (makeCircuit $ insts ++ [Assignment "b" (circuit ! "a")]) ! "a"
