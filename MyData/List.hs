module MyData.List ( listToCombinationPairs ) where

import Control.Monad
import Data.List

-- The following pointfree-fu uses the fact that
--     \x -> f (g x) (h x)
-- is equivalent to
--     f <$> g <*> h
-- in applicative notation.

-- The function itself is modified from this StackExchange answer:
--     http://stackoverflow.com/a/25900462

listToCombinationPairs :: [a] -> [(a,a)]
listToCombinationPairs = join . ((zipWith (zip . repeat)) <$> id <*> (tail . tails))
