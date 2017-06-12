module GeniusSpec (spec) where

import Genius

import Test.Hspec
-- import Test.Hspec.QuickCheck
import Control.Exception (evaluate)


spec :: Spec
spec = do
  describe "nothing" $ do
    it "can calculate" $ do
      1 `shouldBe` 1
--     describe "parser" $ do
--         it "can parse valid booleans" $ do
--              runOne "#t" `shouldBe` "#t"
--              runOne "#f" `shouldBe` "#f"
--         it "cannot parse invalid booleans" $ do
--             runOne "True" `shouldStartWith` "INVALID:"
--         it "can parse valid numbers" $ do
--              runOne "0" `shouldBe` "0"
--              runOne "33" `shouldBe` "33"
--         -- it "cannot parse invalid numbers" $ do
--         --      runOne "123abc" `shouldStartWith` "No match"
--         it "can parse valid symbols" $ do
--              runOne "'quote" `shouldBe` "quote"
--         it "cannot parse invalid symbols" $ do
--              evaluate(runOne "symbol") `shouldThrow` anyException
--         it "can parse valid strings" $ do
--              runOne "\"string\"" `shouldBe` "\"string\""
--              runOne "\"str ing\"" `shouldBe` "\"str ing\""
--         it "cannot parse invalid strings" $ do
--              runOne "\"imbalanced str" `shouldStartWith` "\"No match"
--         it "can parse valid lists" $ do
--              runOne "'()" `shouldBe` "()"
--              runOne "'(1 2)" `shouldBe` "(1 2)"
--              runOne "'(a (nested list))" `shouldBe` "(a (nested list))"
--              runOne "'(a (dotted . list))" `shouldBe` "(a (dotted . list))"
--         it "cannot parse invalid lists" $ do
--              runOne "'(an (imbalanced list)" `shouldStartWith` "\"No match"
--              runOne "'(an (badly . dotted . list))" `shouldStartWith` "\"No match"
--     describe "primitives" $ do
--       it "can perform basic arithmetic" $ do
--         runOne "(+ 2)" `shouldBe` "2"
--         runOne "(+ 2 2)" `shouldBe` "4"
--         runOne "(+ 1 2 3)" `shouldBe` "6"
--         runOne "(* 1)" `shouldBe` "1"
--         runOne "(* 1 3)" `shouldBe` "3"
--         runOne "(* 1 3 9)" `shouldBe` "27"
--         runOne "(/ 16)" `shouldBe` "16"
--         runOne "(/ 16 2)" `shouldBe` "8"
--         runOne "(/ 16 2 2)" `shouldBe` "4"
--         runOne "(- 1)" `shouldBe` "1"
--         runOne "(- 1 1)" `shouldBe` "0"
--         runOne "(- 1 1 1)" `shouldBe` "-1"
--         runOne "(mod 25)" `shouldBe` "25"
--         runOne "(mod 25 7)" `shouldBe` "4"
--         runOne "(mod 25 7 3)" `shouldBe` "1"
--         runOne "(quotient 25)" `shouldBe` "25"
--         runOne "(quotient 25 7)" `shouldBe` "3"
--         runOne "(quotient 25 7 3)" `shouldBe` "1"
--         runOne "(remainder 25)" `shouldBe` "25"
--         runOne "(remainder 25 9)" `shouldBe` "7"
--         runOne "(remainder 25 9 3)" `shouldBe` "1"
--       it "can handle arithmetic errors" $ do
--         evaluate (runOne "(/ 16 0)") `shouldThrow` anyException

--   -- define properties here
-- -- ("=", numBoolBinop (==)),
-- -- ("<", numBoolBinop (<)),
-- -- (">", numBoolBinop (>)),
-- -- ("/=", numBoolBinop (/=)),
-- -- (">=", numBoolBinop (>=)),
-- -- ("<=", numBoolBinop (<=)),
-- -- ("&&", boolBoolBinop (&&)),
-- -- ("||", boolBoolBinop (||)),
-- -- ("string=?", strBoolBinop (==)),
-- -- ("string<?", strBoolBinop (<)),
-- -- ("string>?", strBoolBinop (>)),
-- -- ("string<=?", strBoolBinop (<=)),
-- -- ("string>=?", strBoolBinop (>=)),
