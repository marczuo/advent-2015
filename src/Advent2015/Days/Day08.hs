module Advent2015.Days.Day08 ( part1, part2 ) where

import Control.Monad
import Data.List
import Data.Function
import Data.Either.Unwrap
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P
import Advent2015.Parsec.Combinator

anyHexit, anyUnicode, anyEscaped :: Parsec String () Char 
anyLiteral, anyEscapable :: Parsec String () String

anyHexit = P.oneOf "0123456789abcdef"
anyUnicode = P.string "\\x" >> P.count 2 anyHexit >> return '#'
anyEscaped = P.char '\\' >> P.anyChar >> return '#' 
anyLiteral = P.many1 P.alphaNum
anyEscapable = liftM (('\\':) . return) P.anyChar

strParser1 :: Parsec String () String
strParser2 :: Parsec String () String
compareString :: Parsec String () String -> String -> Int

strParser1 = P.char '"' *> manyChoice [anyUnicode, anyEscaped, P.letter] <* P.char '"' 
strParser2 = do
    pieces <- manyChoice [anyLiteral, anyEscapable]
    return $ "\"" ++ concat pieces ++ "\"" 
compareString parser str = abs $ (subtract`on`length) parsed str where
    parsed = fromRight $ parse parser str str

parseContent = lines
compareWithParsed parser = sum . map (compareString parser)
part1 = compareWithParsed strParser1 . parseContent
part2 = compareWithParsed strParser2 . parseContent
