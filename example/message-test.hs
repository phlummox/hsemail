module Main (main) where

import Text.ParserCombinators.Parsec ( parse )
import Text.ParserCombinators.Parsec.Rfc2822NS

-- Read an Internet message from standard input, parse it,
-- and return the result.

parseEmail s = do
  input <- readFile s
  print $ parse message "<stdin>" input
  return ()

