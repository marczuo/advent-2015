import System.Environment
import System.IO
import Control.Monad

import Data.List.Split (splitOn)

wrap1 :: [Int] -> Int
wrap1 [l,w,h] = 2 * (sum surfaces) + minimum surfaces where
    surfaces = [l*w, w*h, h*l]

strToLWH :: String -> [Int]
strToLWH = map read . splitOn "x"

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = lines content in
              print $ sum $ map (wrap1 . strToLWH) input
