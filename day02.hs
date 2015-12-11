import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Local.IO.AdventOfCode
import Local.Data.List

today = "2"

-- part 1
--
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
    surfaces = map (uncurry (*)) . combinationNoDiag

-- part 2

volume :: [Int] -> Int
volume = foldl (*) 1

-- Readable (but less general, since it only applies to 3 dimensions) version is
--     ribbon [l,w,h] = minimum [2*(l+w),2*(w+h),2*(l+h)]

ribbon :: [Int] -> Int
ribbon = minimum . perimeters where
    perimeters = map ((2*) . (uncurry (+))) . combinationNoDiag


strToDimensions :: String -> [Int]
strToDimensions = map read . splitOn "x"

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = lines
    getRibbonLength = (+) <$> volume <*> ribbon
    part1 = sum . map (wrapGift . strToDimensions)
    part2 = sum . map (getRibbonLength . strToDimensions)
