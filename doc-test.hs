{-
   Module      :  Main
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   HsEmail doctest suite.
-}

module Main ( main ) where

import Test.DocTest

main :: IO ()
main = doctest
       [ "Text/ParserCombinators/Parsec/Rfc2234NS.hs"
       , "Text/ParserCombinators/Parsec/Rfc2822NS.hs"
       ]
