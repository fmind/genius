-- | An initial module.

module Genius
    ( calculate
    ) where

{- |
    Perform a simple calculation.

    >>> calculate 1
    2
    >>> calculate 10
    11
-}
calculate :: Int -> Int
calculate = succ
