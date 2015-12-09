import System.Environment
import System.IO
import Control.Monad
import Data.List
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as Parsec
import Local.IO.AdventOfCode

today = "8"

anyHexit, anyUnicode, anyEscaped :: Parsec String () Char 
anyHexit = Parsec.oneOf "0123456789abcdef"
anyUnicode = Parsec.string "\\x" >> Parsec.count 2 anyHexit >> return '#'
anyEscaped = Parsec.char '\\' >> Parsec.anyChar >> return '#'

anyLiteral, anyEscapable :: Parsec String () String
anyLiteral = Parsec.many1 Parsec.alphaNum
anyEscapable = Parsec.anyChar >>= return . (\x -> ['\\',x])

strParser1 :: Parsec String () String
strParser1 = do
    prefix <- Parsec.char '"'
    innerString <- Parsec.many1 $ Parsec.choice $ map Parsec.try [
        anyUnicode,anyEscaped,Parsec.letter]
    suffix <- Parsec.char '"'
    return innerString

strParser2 :: Parsec String () String
strParser2 = do
    pieces <- Parsec.many1 $ Parsec.choice $ map Parsec.try [anyLiteral, anyEscapable]
    return $ "\"" ++ (concat pieces) ++ "\""

compareString :: Parsec String () String -> String -> Int
compareString parser str = abs $ (length str) - (length parsed) where
    parsed = case (parse parser str str) of
               Left err -> error $ show err
               Right val -> val 

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = lines
    part1 = foldl (+) 0 . map (compareString strParser1)
    part2 = foldl (+) 0 . map (compareString strParser2)
