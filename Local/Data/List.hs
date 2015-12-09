module Local.Data.List ( combination, combinationNoDiag ) where

import Control.Monad
import Data.List

-- The following pointfree-fu uses the fact that
--     \x -> f x (g x)
-- is equivalent to
--     g >>= flip f
-- in monadic notation.

-- The function itself is modified from this StackExchange answer:
--     http://stackoverflow.com/a/25900462

combination :: [a] -> [(a,a)]
combination = join . (tails >>= flip (zipWith (zip . repeat)))

combinationNoDiag :: [a] -> [(a,a)]
combinationNoDiag = join . (tail . tails >>= flip (zipWith (zip . repeat)))
