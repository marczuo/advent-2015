import Control.Parallel.Strategies
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import qualified Data.Digest.Pure.MD5 as MD5
import Local.IO.AdventOfCode
import Local.Data.List

today = "4"

md5AsString :: String -> String
md5AsString = show . MD5.md5 . BSChar8.pack

md5StartsWithNZeros :: Int -> String -> Bool
md5StartsWithNZeros = (. md5AsString) . isPrefixOf . flip replicate '0'

findAnswerN :: Int -> String -> Int
findAnswerN n prefix = length $ takeWhile (==False) $ map (md5StartsWithNZeros n . (prefix++) . show) [0..]

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = head . lines
    part1 = findAnswerN 5
    part2 = findAnswerN 6
