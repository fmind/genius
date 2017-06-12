module Main (main) where

import qualified GeniusBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "Genius" GeniusBench.benchmarks
    ]
