module Main where

import Data.Foldable
import Data.Int
import Data.List
import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Zipper" $ do
    it "fails to go left on a fresh zipper" $
      left (mkZipper ((1 :: Int) :|| 2 :| 3 :| Empty)) `shouldBe` Nothing
    it "goes right on a fresh zipper" $
      right (mkZipper ((1 :: Int) :|| 2 :| 3 :| Empty)) `shouldBe`
        Just (Zipper ((1 :: Int) :| Empty) (2 :|| 3 :| Empty))
    it "converts back and forth" $
      (mkZipper . fromZipper . mkZipper $ ((1 :: Int) :|| 2 :| 3 :| Empty))
        `shouldBe` Zipper Empty ((1 :: Int) :|| 2 :| 3 :| Empty)
  describe "iterInput" $ do
    it "performs a very simple iteration" $ do
      let zipInput = Zipper Empty (Left (0 :: Int16) :|| Empty)
          rules = ((0 :: Int16) :- (0 :: Int8)) :|| Empty
          iterRes = iterInput right zipInput rules Empty
      iterRes `shouldBe` (Right 0 :|| Empty) :| Empty
  describe "iterRules" $ do
    it "performs a very simple iteration" $ do
      let zipInput = Zipper Empty (Left (0 :: Int16) :|| Empty)
          zipRules = Zipper Empty (((0 :: Int16) :- (0 :: Int8)) :|| Empty)
          iterRes = iterRules right zipInput zipRules Empty
      iterRes `shouldBe` (Right 0 :|| Empty) :| Empty
  describe "prependRuleMatches" $ do
    it "accepts matches" $ do
      let rule = (0 :: Int16) :- (0 :: Int8)
          prependation = prependRuleMatches rule (Left 0) Empty
      prependation `shouldBe` (Right 0 :|| Empty) :| Empty
    it "rejects nonmatches" $ do
      let rule = (0 :: Int16) :- (0 :: Int8)
          prependation = prependRuleMatches rule (Left 1) Empty
      prependation `shouldBe` Empty
  describe "generate'" $ do
    it "performs a no-op iteration" $ do
      let rule = pure $ (0 :: Int16) :- (0 :: Int8)
      generate' rule (pure $ Left 0 :|| Empty) 0 `shouldBe` (0 :|| Empty) :| Empty
  describe "generate" $ do
    it "creates the empty output" $
      let grammar =
            Grammar
              { grammarRules =
                  ((0 :: Int16) := (1 :: Int16, 2 :: Int16)) :||
                    (1 :- (3 :: Int8)) :|
                    (2 :- 4) :|
                    Empty
              , grammarStart = 0
              , grammarEmptyString = 0
              }
       in (sort . toList) (generate grammar 5) `shouldBe` [(0 :|| Empty), (3 :|| 4 :| Empty)]
