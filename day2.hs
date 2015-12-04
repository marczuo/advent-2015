import System.Environment
import System.IO
import Control.Monad
import Data.List
import IO.ReadApplyPrint

import Data.List.Split (splitOn)

-- The following pointfree-fu uses the fact that
--     \x -> f (g x) (h x)
-- is equivalent to
--     liftM2 f g h
-- (which can be written as f <$> g <*> h)
-- by making f into a monad and applying it to g, h.
--
-- A more readable version (but which only works on 3 edges) is
--     wrapGifts :: [Int] -> Int
--     wrapGifts [l,w,h] = 2 * (sum surfaces) + minimum surfaces where
--         surfaces = [l*w, w*h, h*l]

wrapGift :: [Int] -> Int
wrapGift = ((+) <$> ((*2) . sum) <*> minimum) . surfaces where
    surfaces = map (uncurry (*)) . pairingList
    pairingList = join . ((zipWith (zip . repeat)) <$> id <*> (tail . tails))

strToLWH :: String -> [Int]
strToLWH = map read . splitOn "x"

main :: IO ()
main = readApplyPrint argsToFileName parseContent findAnswer where
    argsToFileName = head
    parseContent = lines
    findAnswer = sum . map (wrapGift . strToLWH)
