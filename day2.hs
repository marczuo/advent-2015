import System.Environment
import System.IO
import Control.Monad
import IO.ReadApplyPrint

import Data.List.Split (splitOn)

wrap1 :: [Int] -> Int
wrap1 [l,w,h] = 2 * (sum surfaces) + minimum surfaces where
    surfaces = [l*w, w*h, h*l]

strToLWH :: String -> [Int]
strToLWH = map read . splitOn "x"

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = sum . map (wrap1 . strToLWH)
