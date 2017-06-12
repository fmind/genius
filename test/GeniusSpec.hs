module GeniusSpec (spec) where

import Genius

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "calculate" $ do
        it "returns a defined int" $ do
            calculate 0 `shouldBe` 1

        prop "always returns the next int" $
            \ x -> calculate x > x
