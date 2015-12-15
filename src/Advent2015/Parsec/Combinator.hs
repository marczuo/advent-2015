module Advent2015.Parsec.Combinator ( manyChoice ) where

import Text.Parsec

manyChoice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m [a]
manyChoice = many1 . choice . map try
