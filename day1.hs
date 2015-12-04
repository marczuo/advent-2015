import System.Environment
import System.IO
import Control.Monad
import IO.ReadApplyPrint

instsToFloor :: String -> Int
instsToFloor "" = 0
instsToFloor ('(':xs) = (instsToFloor xs) + 1
instsToFloor (')':xs) = (instsToFloor xs) - 1

main :: IO ()
main = readApplyPrint argsToFileName parseContent instsToFloor where
    argsToFileName = head
    parseContent = head . lines
