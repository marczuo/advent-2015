module Advent2015.Days.Day16 ( part1, part2 ) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Function
import Safe
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Either.Unwrap
import Text.Parsec (parse, Parsec)
import qualified Text.Parsec as P

-- Data structure

type Sue = Map String Int

known = M.fromList ([("children", 3), ("cats", 7), ("samoyeds", 2),
                     ("pomeranians", 3), ("akitas", 0), ("vizslas", 0),
                     ("goldfish", 5), ("trees", 3), ("cars", 2),
                     ("perfumes", 1)] :: [(String, Int)])

-- Logic

isRightSue :: (Sue -> Sue -> Bool) -> Sue -> Sue -> Bool
isRightSue comparer profile sue = M.intersection profile sue `comparer` M.intersection sue profile

rectify :: Sue -> Sue -> Bool
rectify s p = go (sortBy (compare `on` fst) $ M.toList p)
                 (sortBy (compare `on` fst) $ M.toList s) where
    go [] [] = True
    go profile sample = headMatches && go tailProfile tailSample where
        (headProfile:tailProfile) = profile
        (headSample:tailSample)   = sample
        headMatches = case (fst headProfile) of
                        "cats" -> snd headProfile > snd headSample
                        "trees" -> snd headProfile > snd headSample
                        "pomeranians" -> snd headProfile < snd headSample
                        "goldfish" -> snd headProfile < snd headSample
                        otherwise -> snd headProfile == snd headSample

-- Input parsing

lineParser :: Parsec String () [(String, Int)]
fileParser :: Parsec String () [Sue]

lineParser = P.string "Sue " *> P.many1 P.digit *> P.string ": " *>
    let oneThing = (,) <$> P.manyTill P.letter (P.string ": ") <*> (read <$> P.many1 P.digit) in
        P.sepBy oneThing $ P.string ", "

fileParser = P.endBy (M.fromList <$> lineParser) $ P.char '\n'

parseContent content = fromRight $ parse fileParser content content
part1 = headDef 0 . map (+1) . findIndices (isRightSue (==) known) . parseContent
part2 = headDef 0 . map (+1) . findIndices (isRightSue rectify known) . parseContent
