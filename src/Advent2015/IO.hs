module Advent2015.IO ( adventIO, Operation, noopPrim, onlyPart1Prim, onlyPart2Prim ) where 

import System.Environment
import System.IO
import Data.List
import Data.Maybe
import Data.Either.Unwrap
import Safe
import Control.Monad
import Control.Applicative
import Control.Arrow
import Text.Parsec (Parsec, parse)
import qualified Text.Parsec as P

data Operation = Noop | OnlyPart1 | OnlyPart2 | BothParts deriving Eq
noopPrim = Noop; onlyPart1Prim = OnlyPart1; onlyPart2Prim = OnlyPart2

dayFlagParser :: Parsec String () Int
dayFlagParser = P.choice $ map P.try [fullDay, abbrevDay] where
    fullDay = read <$> (P.string "--day" *> P.many1 P.digit)
    abbrevDay = read <$> (P.char '-' *> P.many1 P.digit)

parseDayFlag :: [String] -> Int
parseDayFlag args = let parsed = map (\arg -> parse dayFlagParser arg arg) args
                        matched = filter isRight parsed in
                        if length matched /= 1
                        then (error "Error: Exactly one argument of type --day## or -## is required.")
                        else fromRight $ head matched

adventIO :: IO (Operation, Int, String)
adventIO = let helpMsg = "Advent of code solver\n\n" ++
                        "Usage: Advent2015 [--day01..25|-01..-25] [--part-one-only|-p1] [--part-two-only|-p2] input-file\n" in do 
               args <- getArgs
               let helpFlag  = "--help" `elem` args
                   notAFlag  = not . isPrefixOf "-"
                   fileName  = if noInput then "" else fromMaybe (error "Error: No file name given.") 
                                                                 (headMay $ filter notAFlag args)
                   onlyPart1 = "--part-one-only" `elem` args ||
                               "-p1" `elem` args
                   onlyPart2 = "--part-two-only" `elem` args ||
                               "-p2" `elem` args
                   day       = parseDayFlag args
                   noInput   = day == 10 in
                   if helpFlag then putStr helpMsg >> return (Noop, 0, "")
                   else do
                       input <- if noInput then return "" else readFile fileName 
                       return $ case (onlyPart1, onlyPart2) of
                                  (True, True) -> error "Error: --part-one-only and --part-two-only cannot be used together."
                                  (True, False) -> (OnlyPart1, day, input)
                                  (False, True) -> (OnlyPart2, day, input)
                                  (False, False) -> (BothParts, day, input)
