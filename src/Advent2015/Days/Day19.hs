module Advent2015.Days.Day19 ( part1, part2 ) where

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

type Atom = String
type Molecule = [Atom]
type Rule = (Atom, Molecule)

atoms :: [Atom]
atoms = [[a,b] | a <- ['A'..'Z'], b <- ['a'..'z']]
         ++ map return ['A'..'Z']

-- Logic for part 1

applyRule' :: Molecule -> Rule -> Molecule -> [Molecule]
applyRule' _ _ []                      = []
applyRule' prev (input, output) (a:as) = (if a == input then [prev ++ output ++ as] else [])
                                              ++ applyRule' (prev ++ [a]) (input, output) as
applyRule = applyRule' []

applyAll :: [Rule] -> Molecule -> [Molecule]
applyAll rules input = nub $ flip applyRule input =<< rules

-- Logic for part 2

count a b = length $ filter (==a) b
getFabTime input = (length input) - (count "Rn" input) - (count "Ar" input) - 2*(count "Y" input) - 1

-- Input parsing

atomParser :: Parsec String () Atom
atomParser = P.choice $ map (P.try . P.string) atoms

electronParser :: Parsec String () [Molecule]
electronParser = P.many (P.try $ oneThing <* P.string "\n") where
    oneThing = P.string "e => " *> P.many1 atomParser

ruleParser :: Parsec String () Rule
ruleParser = (,) <$> atomParser <* P.string " => " <*> P.many1 atomParser

fileParser :: Parsec String () ([Rule], [Molecule], Molecule)
fileParser = (,,) <$> P.many (P.try $ ruleParser <* P.string "\n")
                 <*> electronParser
                 <*  P.string "\n"
                 <*> P.many atomParser

part1 content = let (rules, electrons, input) = fromRight $ parse fileParser content content in
                    length $ applyAll rules input
part2 content = let (rules, electrons, input) = fromRight $ parse fileParser content content in
                    getFabTime input
