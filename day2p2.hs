import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint
import MyData.List

import Data.List.Split (splitOn)

volume :: [Int] -> Int
volume = foldl (*) 1

-- Same pointfree-fu in use here, see day2.hs for an explanation
-- Readable (but less general, since it only applies to 3 dimensions) version is
--     ribbon [l,w,h] = minimum [2*(l+w),2*(w+h),2*(l+h)]

ribbon :: [Int] -> Int
ribbon = minimum . perimeters where
    perimeters = map ((2*) . (uncurry (+))) . listToCombinationPairs

strToDimensions :: String -> [Int]
strToDimensions = map read . splitOn "x"

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = sum . map ((\x -> (volume x) + (ribbon x)) . strToDimensions)
