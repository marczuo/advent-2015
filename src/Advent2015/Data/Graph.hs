module Advent2015.Data.Graph ( Graph, Association2, getIndices, makeGraph,
                               makeSymmetricGraph, lookupGraph, nvertices,
                               negateGraph ) where

import Data.Maybe
import Data.List
import Data.Matrix

type Graph val = Matrix val
type Association2 lab val = ((lab,lab),val)

getIndices :: Eq lab => [Association2 lab val] -> [lab]
getIndices = nub . concatMap (\(x,y) -> [x,y]) . fst . unzip

makeGraph :: Eq lab => val -> [Association2 lab val] -> Graph val
makeGraph n g = let indices = getIndices g
                    size    = length indices in
                    matrix size size
                        (\(x,y) ->
                            let xLabel = indices !! (x-1)
                                yLabel = indices !! (y-1) in
                                fromMaybe n (lookup (xLabel, yLabel) g))

makeSymmetricGraph :: Eq lab => val -> [Association2 lab val] -> Graph val
makeSymmetricGraph n g = let indices = getIndices g
                             size    = length indices in
                             matrix size size
                                 (\(x,y) ->
                                    let xLabel = indices !! (x-1)
                                        yLabel = indices !! (y-1) in
                                        fromMaybe (fromMaybe n (lookup (yLabel, xLabel) g))
                                                  (lookup (xLabel, yLabel) g))

nvertices :: Graph val -> Int
nvertices = ncols

lookupGraph :: (Int,Int) -> Graph val -> Maybe val
lookupGraph (x,y) g = Just (getElem x y g)

negateGraph :: Num val => Graph val -> Graph val
negateGraph = scaleMatrix (-1)
