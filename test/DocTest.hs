module Main (main) where

import Test.DocTest (doctest)
import System.FilePath.Glob (glob)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest
