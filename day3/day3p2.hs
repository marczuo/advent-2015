import System.Environment
import System.IO
import Control.Monad

import qualified Data.Set as S
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
    where go _ [] = []
          go s (x:xs) | S.member x s = go s xs
                      | otherwise    = x : go (S.insert x s) xs 

data Coord = Coord Int Int deriving (Show, Eq, Ord)
xOf, yOf :: Coord -> Int
xOf (Coord x _) = x 
yOf (Coord _ y) = y

instsToCoords :: String -> [Coord]
instsToCoords = scanl oneMove (Coord 0 0) where
    oneMove c '<' = Coord ((xOf c)-1) (yOf c)
    oneMove c '>' = Coord ((xOf c)+1) (yOf c)
    oneMove c '^' = Coord (xOf c) ((yOf c)+1)
    oneMove c 'v' = Coord (xOf c) ((yOf c)-1)

splitInsts :: String -> (String, String)
splitInsts "" = ("", "")
splitInsts (a:b:xs) = (a:oldA, b:oldB) where
    (oldA, oldB) = splitInsts xs
splitInsts (a:xs) = (a:oldA, oldB) where
    (oldA, oldB) = splitInsts xs

instsToCoords2 :: String -> [Coord]
instsToCoords2 insts = uncurry (++) (instsToCoords insts1, instsToCoords insts2) where
    (insts1, insts2) = splitInsts insts

instsToCount2 :: String -> Int
instsToCount2 = length . nub' . instsToCoords2

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = head $ lines content in
              print $ instsToCount2 input
