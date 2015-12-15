import Control.Monad
import Control.Applicative
import Data.Maybe
--import Data.Either.Unwrap
import Text.Parsec (parse, Parsec)
import qualified Text.Parsec as P
import Local.IO.AdventOfCode

today = "15"

-- Data structure

type Ingredient = (Int, Int, Int, Int, Int)

-- Logic

-- Input parsing

lineParser :: Parsec String () Ingredient
fileParser :: Parsec String () [Ingredient]

lineParser = let number = read <$> P.many (P.oneOf ('-':['0'..'9'])) in
                 (,,,,) <$ P.manyTill P.letter (P.char ':') <* P.spaces
                        <* P.string "capacity " <*> number <* P.string ", " 
                        <* P.string "durability " <*> number <* P.string ", "
                        <* P.string "flavor " <*> number <* P.string ", "
                        <* P.string "texture " <*> number <* P.string ", "
                        <* P.string "calories " <*> number

fileParser = P.endBy lineParser $ P.char '\n'

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent content = parse fileParser content content
    part1 = id
    part2 = id
