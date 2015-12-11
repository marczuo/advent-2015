import Control.Monad
import Data.List
import Data.Function
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P
import Local.IO.AdventOfCode
import Local.Data.Either
import Local.Parsec.Combinator

today = "8"

anyHexit, anyUnicode, anyEscaped :: Parsec String () Char 
anyHexit = P.oneOf "0123456789abcdef"
anyUnicode = P.string "\\x" >> P.count 2 anyHexit >> return '#'
anyEscaped = P.char '\\' >> P.anyChar >> return '#'

anyLiteral, anyEscapable :: Parsec String () String
anyLiteral = P.many1 P.alphaNum
anyEscapable = liftM (('\\':) . return) P.anyChar

strParser1 :: Parsec String () String
strParser1 = do
    P.char '"'
    innerString <- manyChoice [anyUnicode, anyEscaped, P.letter]
    P.char '"'
    return innerString

strParser2 :: Parsec String () String
strParser2 = do
    pieces <- manyChoice [anyLiteral, anyEscapable]
    return $ "\"" ++ concat pieces ++ "\""

compareString :: Parsec String () String -> String -> Int
compareString parser str = abs $ (subtract`on`length) parsed str where
    parsed = errorOnLeft $ parse parser str str

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = lines
    compareWithParsed parser = sum . map (compareString parser)
    part1 = compareWithParsed strParser1
    part2 = compareWithParsed strParser2
