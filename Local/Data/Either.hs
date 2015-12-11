module Local.Data.Either ( errorOnLeft ) where

errorOnLeft :: Show a => Either a b -> b
errorOnLeft either = case either of
                       Left msg -> error $ show msg
                       Right thing -> thing
