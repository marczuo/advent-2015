import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Local.IO.AdventOfCode
import Local.Data.List

today = "2"

surfaces, perimeters :: [Int] -> [Int]
surfaces = map (uncurry (*)) . combinationNoDiag
perimeters = map ((2*) . uncurry (+)) . combinationNoDiag

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = lines
    strToDimensions = map read . splitOn "x"
    getRibbonLength1 = ((+) <$> ((*2) . sum) <*> minimum) . surfaces
    getRibbonLength2 = (+) <$> product <*> minimum . perimeters
    part1 = sum . map (getRibbonLength1 . strToDimensions)
    part2 = sum . map (getRibbonLength2 . strToDimensions)
