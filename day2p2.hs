import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

import Data.List.Split (splitOn)

volume :: [Int] -> Int
volume = foldl (*) 1

-- Same pointfree-fu in use here, see day2.hs for an explanation
-- Readable (but less general, since it only applies to 3 edges) version is
--     ribbon [l,w,h] = minimum [2*(l+w),2*(w+h),2*(l+h)]

ribbon :: [Int] -> Int
ribbon = minimum . perimeters where
    perimeters = map ((2*) . (uncurry (+))) . pairingList
    pairingList = join . ((zipWith (zip . repeat)) <$> id <*> (tail . tails))

strToLWH :: String -> [Int]
strToLWH = map read . splitOn "x"

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = sum . map ((\x -> (volume x) + (ribbon x)) . strToLWH)
