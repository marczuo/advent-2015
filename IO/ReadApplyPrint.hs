module IO.ReadApplyPrint ( readApplyPrint ) where 

import System.Environment
import System.IO

readApplyPrint :: Show b => ([String] -> String) -> (String -> a) -> (a -> b) -> IO ()
readApplyPrint argsToFileName parseContent function = getArgs >>= readFile . argsToFileName
                                                              >>= print . function . parseContent
