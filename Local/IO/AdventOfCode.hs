module Local.IO.AdventOfCode ( adventIO ) where 

import System.Environment
import System.IO
import Data.List
import Control.Monad
import Control.Arrow

adventIO :: Show b => Show c => String -> (String -> a) -> (a -> b) -> (a -> c) -> IO ()
adventIO day parseContent part1 part2 = 
    let safeHead [] = Nothing
        safeHead (x:xs) = Just x 
        helpMsg = "Advent of code day " ++ day ++ " solver\n\n" ++
                  "Usage: day" ++ day ++ " [--part-one-only|-p1] [--part-two-only|-p2] input-file\n" in do 
            args <- getArgs
            let helpFlag  = "--help" `elem` args
                notAFlag  = not . isPrefixOf "-"
                fileName  = case safeHead (filter notAFlag args) of
                              Nothing -> error "Error: No file name given."
                              Just name -> name
                onlyPart1 = "--part-one-only" `elem` args ||
                            "-p1" `elem` args
                onlyPart2 = "--part-two-only" `elem` args ||
                            "-p2" `elem` args in
                if helpFlag then putStr helpMsg
                else do
                    input <- parseContent <$> (readFile fileName)
                    when (onlyPart1 && onlyPart2) $
                        error "Error: --part-one-only and --part-two-only cannot be used together."
                    when (not onlyPart2) $ putStr ("day" ++ day ++ "p1: ") >> print (part1 input)
                    when (not onlyPart1) $ putStr ("day" ++ day ++ "p2: ") >> print (part2 input)
