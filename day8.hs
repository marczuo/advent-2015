import System.Environment
import System.IO
import Control.Monad
import Data.List
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as Parsec
import IO.ReadApplyPrint

anyHexit, anyUnicode, anyEscaped :: Parsec String () Char 
anyHexit = Parsec.oneOf "0123456789abcdef"
anyUnicode = Parsec.string "\\x" >> Parsec.count 2 anyHexit >> return '#'
anyEscaped = Parsec.char '\\' >> Parsec.anyChar >> return '#'

strParser :: Parsec String () String
strParser = do
    prefix <- Parsec.char '"'
    innerString <- Parsec.many1 $ Parsec.choice $ map Parsec.try [
        anyUnicode,anyEscaped,Parsec.letter]
    suffix <- Parsec.char '"'
    return innerString

compareString :: String -> Int
compareString str = (length str) - (length parsed) where
    parsed = case (parse strParser str str) of
               Left err -> error $ show err
               Right val -> val

compareStrings :: [String] -> Int
compareStrings = foldl (+) 0 . map compareString

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = compareStrings
