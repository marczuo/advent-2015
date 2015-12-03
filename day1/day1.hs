import System.Environment
import System.IO
import Control.Monad

instsToFloor :: String -> Int
instsToFloor "" = 0
instsToFloor ('(':xs) = (instsToFloor xs) + 1
instsToFloor (')':xs) = (instsToFloor xs) - 1

main = do args <- getArgs
          content <- readFile (args !! 0)
          let input = (lines content) !! 0 in
              print $ instsToFloor input
