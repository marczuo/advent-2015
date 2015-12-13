module Local.Data.Maybe ( minMaybe, maxMaybe, applyToJusts ) where

import Data.Maybe

minMaybe :: Ord a => [Maybe a] -> Maybe a
maxMaybe :: Ord a => [Maybe a] -> Maybe a

minMaybe = applyToJusts minimum
maxMaybe = applyToJusts maximum 

applyToJusts :: ([a] -> b) -> [Maybe a] -> Maybe b
applyToJusts func list = case catMaybes list of
                           [] -> Nothing
                           nonEmpty -> Just (func nonEmpty)
