module Local.Data.List ( combination, combinationNoDiag, slice ) where

import Control.Monad
import Data.List

-- The function is from this StackExchange answer:
--     http://stackoverflow.com/a/25900462

combination :: [a] -> [(a,a)]
combination = join . (zipWith (zip . repeat) `ap` tails)

combinationNoDiag :: [a] -> [(a,a)]
combinationNoDiag = join . (zipWith (zip . repeat) `ap` (tail . tails))

slice :: [a] -> Int -> Int -> [a]
slice list i n = take n $ drop i list
