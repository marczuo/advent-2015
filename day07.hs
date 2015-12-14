import Control.Monad
import Data.List
import Data.Bits
import Data.Either.Unwrap
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P
import Local.IO.AdventOfCode

today = "7"

-- Data Structure

type UnaryOperation = Int -> Int 
type BinaryOperation = Int -> Int -> Int
type Circuit = Map String Int
type ParString a = Parsec String () a

data Instruction = Assignment String Int
                 | Unary UnaryOperation String String
                 | Binary BinaryOperation String String String

-- Logic & Input parsing

anyNumber, anyLower, anyUpper :: ParString String
assignmentParser :: ParString a -> ParString b -> ParString (a,b)
unaryParser :: ParString a -> ParString b -> ParString c -> ParString (a,b,c)
binaryParser :: ParString a -> ParString b -> ParString c -> ParString d -> ParString (a,b,c,d)
assignmentLiteral, assignment, unary, binary :: ParString Instruction
binaryRightLiteral, binaryLeftLiteral :: ParString Instruction

anyNumber = P.many1 P.digit
anyLower = P.many1 P.lower
anyUpper = P.many1 P.upper

assignmentParser parserR parserL = (,) <$> parserR <* P.string " -> " <*> parserL

unaryParser parserOp parserR parserL = (,,) <$> parserOp <* P.spaces <*> parserR
                                            <* P.string " -> " <*> parserL

binaryParser parserR1 parserOp parserR2 parserL = (,,,) <$> parserR1 <* P.spaces
                                                        <*> parserOp <* P.spaces
                                                        <*> parserR2 <* P.string " -> "
                                                        <*> parserL

assignment = do
    (tokenR, tokenL) <- assignmentParser anyLower anyLower
    return $ Unary id tokenL tokenR

assignmentLiteral = do
    (tokenR, tokenL) <- assignmentParser anyNumber anyLower
    return $ Assignment tokenL (read tokenR) 

unary = do
    (_, tokenR, tokenL) <- unaryParser anyUpper anyLower anyLower
    return $ Unary complement tokenL tokenR

binary = do
    (tokenR1, tokenOp, tokenR2, tokenL) <- binaryParser anyLower anyUpper anyLower anyLower
    return $ case tokenOp of
               "LSHIFT" -> Binary shift tokenL tokenR1 tokenR2
               "RSHIFT" -> Binary (\x y -> x `shift` (-y)) tokenL tokenR1 tokenR2
               "AND" -> Binary (.&.) tokenL tokenR1 tokenR2
               "OR"  -> Binary (.|.) tokenL tokenR1 tokenR2

binaryRightLiteral = do
    (tokenR1, tokenOp, tokenR2, tokenL) <- binaryParser anyLower anyUpper anyNumber anyLower
    return $ case tokenOp of
               "LSHIFT" -> Unary (`shift` read tokenR2) tokenL tokenR1
               "RSHIFT" -> Unary (`shift` (-(read tokenR2))) tokenL tokenR1 
               "AND" -> Unary (.&. read tokenR2) tokenL tokenR1
               "OR" -> Unary (.|. read tokenR2) tokenL tokenR1 

binaryLeftLiteral = do
    (tokenR1, tokenOp, tokenR2, tokenL) <- binaryParser anyNumber anyUpper anyLower anyLower
    return $ case tokenOp of
               "LSHIFT" -> Unary (shift (read tokenR1)) tokenL tokenR2
               "RSHIFT" -> Unary (shift (-(read tokenR1))) tokenL tokenR2 
               "AND" -> Unary (read tokenR1 .&.) tokenL tokenR2
               "OR" -> Unary (read tokenR1 .|.) tokenL tokenR2 

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
parseFile content = fromRight $ parse (P.endBy lineParser $ P.char '\n') content content

main :: IO ()
main = adventIO today parseContent part1 part2 where
    argsToFileName = head
    parseContent content = let insts = parseFile content in (makeCircuit insts,insts)
    part1 (circuit,insts) = circuit ! "a"
    part2 (circuit,insts) = makeCircuit (insts ++ [Assignment "b" (circuit ! "a")]) ! "a"
