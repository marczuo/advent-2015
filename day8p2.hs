import System.Environment
import System.IO
import Control.Monad
import Data.List
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as Parsec
import IO.ReadApplyPrint

anyLiteral, anyEscapable :: Parsec String () String
anyLiteral = Parsec.many1 Parsec.alphaNum
anyEscapable = Parsec.anyChar >>= return . (\x -> ['\\',x])

strParser :: Parsec String () [String]
strParser = Parsec.many1 $ Parsec.choice $ map Parsec.try [anyLiteral, anyEscapable]

compareString :: String -> Int
compareString str = (length parsed) - (length str) where
    parsed = case (parse strParser str str) of
               Left err -> error $ show err
               Right val -> "\"" ++ (concat val) ++ "\""

compareStrings :: [String] -> Int
compareStrings = foldl (+) 0 . map compareString

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = compareStrings
