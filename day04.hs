import Control.Monad
import Control.Parallel.Strategies
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import qualified Data.Digest.Pure.MD5 as MD5
import Local.IO.AdventOfCode

today = "4"

slice :: [a] -> Int -> Int -> [a]
slice list i n = take n $ drop i list

md5AsString :: String -> String
md5AsString = show . MD5.md5 . BSChar8.pack

md5StartsWithNZeros :: Int -> String -> Bool
md5StartsWithNZeros n str = isPrefixOf (replicate n '0') (md5AsString str)

-- Brute force search. This is really really slow.
-- Unfortunately I'm not knowledgable enough about MD5 to implement an efficient search.
findAnswerN :: Int -> String -> Int
findAnswerN n prefix = go 0 where
    chunkSize = 200
    go start = let chunk = parMap rpar ((md5StartsWithNZeros n) . (prefix++) . show) [start..start+chunkSize]
                   count = length $ takeWhile (==False) chunk in
                   if count == chunkSize+1 then go (start+chunkSize) else start+count

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = head . lines
    part1 = findAnswerN 5
    part2 = findAnswerN 6