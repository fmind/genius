module GeniusBench (benchmarks) where

import Genius

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "calculate 1" $ whnf calculate 1
    ]
