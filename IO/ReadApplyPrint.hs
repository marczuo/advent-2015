module IO.ReadApplyPrint ( readApplyPrint, readApplyPutStr ) where 

import System.Environment
import System.IO

readApplyPrint :: Show b => ([String] -> String) -> (String -> a) -> (a -> b) -> IO ()
readApplyPrint argsToFileName parseContent function = getArgs >>= readFile . argsToFileName
                                                              >>= print . function . parseContent

readApplyPutStr :: ([String] -> String) -> (String -> a) -> (a -> String) -> IO ()
readApplyPutStr argsToFileName parseContent function = getArgs >>= readFile . argsToFileName
                                                              >>= putStr . function . parseContent
