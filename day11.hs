import Control.Monad
import Control.Arrow
import Data.Char
import Safe
import Data.List
import Data.List.Extra
import Data.Maybe
import Local.IO.AdventOfCode

today = "11"

nextChar :: Char -> (Char, Bool)
nextChar char | char == 'z' = ('a', True)
              | char == 'h' = ('j', False)
              | char == 'n' = ('p', False)
              | char == 'k' = ('m', False)
              | otherwise   = (succ char, False)

incrementHelper :: String -> (String, Bool)
incrementHelper str = case str of
                        []      -> ([], False)
                        [aChar] -> first return $ nextChar aChar
                        c:cs    -> let (ncs, carry) = incrementHelper cs
                                       (nc, ncarry) = if carry then nextChar c else (c, False) in
                                       (nc:ncs, ncarry)

increment :: String -> String
increment str = let (result, carry) = incrementHelper str in
                    if carry then error "Exhausted possible passwords"
                             else result

isNice :: String -> Bool
isNice str = runOfThree && twoPairs where
    charsInStr = nubOrd str
    runOfThree = True `elem` (map (`isInfixOf` str) [map chr [x,x+1,x+2] | x <- map ord charsInStr])
    twoPairs = not $ null $ tailSafe $ filter (`isInfixOf` str) [[char,char] | char <- charsInStr]

getPwdSequence :: String -> [String]
getPwdSequence str = filter isNice seq where seq = str : (map increment seq)

main :: IO ()
main = adventIO today parseContent part1 part2 where
    parseContent = getPwdSequence . head . lines
    part1 = (!! 0)
    part2 = (!! 1)
