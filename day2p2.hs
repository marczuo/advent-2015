import System.Environment
import System.IO
import Control.Monad
import IO.ReadApplyPrint

import Data.List.Split (splitOn)

volume :: [Int] -> Int
volume = foldl (*) 1

ribbon :: [Int] -> Int
ribbon [l,w,h] = minimum [2*(l+w),2*(w+h),2*(l+h)]

strToLWH :: String -> [Int]
strToLWH = map read . splitOn "x"

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = sum . map ((\x -> (volume x) + (ribbon x)) . strToLWH)
