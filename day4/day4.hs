import           System.Environment
import           System.IO
import           Control.Monad
import           Data.List
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import qualified Data.Digest.Pure.MD5 as MD5

md5AsString :: String -> String
md5AsString = show . MD5.md5 . BSChar8.pack

md5StartsWithZeros :: String -> Bool
md5StartsWithZeros = isPrefixOf "00000" . md5AsString

findAnswer :: String -> Int
findAnswer prefix = length $ takeWhile (not . md5StartsWithZeros) (map (((++) prefix) . show) [0..])

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = head . lines

    applyAndPrintResult :: Show b => (a -> b) -> a -> IO ()
    applyAndPrintResult function = print . function 

    readApplyPrint :: Show b => ([String] -> String) -> (String -> a) -> (a -> b) -> IO ()
    readApplyPrint argsToFileName parseContent function = getArgs >>= readFile . argsToFileName
                                                                  >>= applyAndPrintResult function . parseContent
