import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint
import MyData.List

import Data.List.Split (splitOn)

-- The following pointfree-fu uses the fact that
--     \x -> f (g x) (h x)
-- is equivalent to the applicative notation
--     f <$> g <*> h
--
-- A more readable version (but which only works in 3 dimensions) is
--     wrapGifts :: [Int] -> Int
--     wrapGifts [l,w,h] = 2 * (sum surfaces) + minimum surfaces where
--         surfaces = [l*w, w*h, h*l]

wrapGift :: [Int] -> Int
wrapGift = ((+) <$> ((*2) . sum) <*> minimum) . surfaces where
    surfaces = map (uncurry (*)) . listToCombinationPairs

strToDimensions :: String -> [Int]
strToDimensions = map read . splitOn "x"

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = sum . map (wrapGift . strToDimensions)
