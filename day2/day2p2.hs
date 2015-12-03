import System.Environment
import System.IO
import Control.Monad

import Data.List.Split (splitOn)

volume :: [Int] -> Int
volume = foldl (*) 1

ribbon :: [Int] -> Int
ribbon [l,w,h] = minimum [2*(l+w),2*(w+h),2*(l+h)]

strToLWH :: String -> [Int]
strToLWH = map read . splitOn "x"

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = lines content
              solutions = map ((\x -> (volume x) + (ribbon x)) . strToLWH) input in
              print $ sum solutions
