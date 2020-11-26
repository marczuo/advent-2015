module Advent2015.Days.Day21 ( part1, part2 ) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Function
import Data.Ord
import Safe
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Either.Unwrap
import Text.Parsec (parse, Parsec)
import qualified Text.Parsec as P

-- Data structure

data Item = Item { cost :: Integer
                 , damage :: Integer
                 , armor :: Integer
                 } deriving (Show)

weapons = [ Item 8 4 0
          , Item 10 5 0
          , Item 25 6 0
          , Item 40 7 0
          , Item 74 8 0
          ]
armors  = [ Item 13 0 1
          , Item 31 0 2
          , Item 53 0 3
          , Item 75 0 4
          , Item 102 0 5
          , Item 0 0 0
          ]
rings   = [ Item 25 1 0
          , Item 50 2 0
          , Item 100 3 0
          , Item 20 0 1
          , Item 40 0 2
          , Item 80 0 3
          , Item 0 0 0
          , Item 0 0 0
          ]

loadouts :: [[Item]]
loadouts = [[wpn] ++ [armor] ++ rngs | wpn <- weapons, armor <- armors, rngs <- choose 2 rings] where
    choose n list = concatMap permutations $ choose' list [] where
        choose' []     r = if length r == n then [r] else []
        choose' (x:xs) r | length r == n = [r]
                         | otherwise     = choose' xs (x:r) 
                                           ++ choose' xs r

-- Logic

myHp = 100 :: Integer 

getRounds :: Integer -> Integer -> Integer -> Maybe Integer
getRounds hp amr dmg = if dmg - amr > 0 then Just $ hp `div` (dmg - amr)
                                        else Nothing

calcCost loadout = sum $ map cost loadout
calcDmg loadout = sum $ map damage loadout
calcArmor loadout = sum $ map armor loadout

winable :: Integer -> Integer -> Integer -> Integer -> [Item] -> Bool
winable myHp bossHp bossDmg bossArmor loadout =
    let cost = calcCost loadout
        damage = calcDmg loadout
        armor = calcArmor loadout
        myRounds = getRounds bossHp bossArmor damage
        bossRounds = getRounds myHp armor bossDmg in
        if isNothing myRounds
           then False
           else if isNothing bossRounds
                then True
                else fromJust myRounds <= fromJust bossRounds

-- Input parsing

fileParser :: Parsec String () (Integer, Integer, Integer)
fileParser = (,,) <$ P.string "Hit Points: " <*> (read <$> P.many1 P.digit) <* P.string "\n"
                  <* P.string "Damage: " <*> (read <$> P.many1 P.digit) <* P.string "\n"
                  <* P.string "Armor: " <*> (read <$> P.many1 P.digit)

part1 content = let (bossHp, bossDmg, bossArmor) = fromRight $ parse fileParser content content
                    outcomes = filter (winable 100 bossHp bossDmg bossArmor) loadouts
                 in calcCost $ minimumBy (comparing calcCost) outcomes
part2 content = let (bossHp, bossDmg, bossArmor) = fromRight $ parse fileParser content content
                    outcomes = filter (not . winable 100 bossHp bossDmg bossArmor) loadouts
                 in calcCost $ maximumBy (comparing calcCost) outcomes
