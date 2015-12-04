import           System.Environment
import           System.IO
import           Control.Monad
import           Data.List
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import qualified Data.Digest.Pure.MD5 as MD5
import           IO.ReadApplyPrint

md5AsString :: String -> String
md5AsString = show . MD5.md5 . BSChar8.pack

md5StartsWithZeros :: String -> Bool
md5StartsWithZeros = isPrefixOf "000000" . md5AsString

findAnswer :: String -> Int
findAnswer prefix = length $ takeWhile (not . md5StartsWithZeros) (map (((++) prefix) . show) [0..])

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = head . lines
