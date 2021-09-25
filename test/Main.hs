module Main where

import Data.Int
import Gnoam.Internal
import Gnoam.Types
import System.Random
import Test.Hspec

-- we need to define this in order to use
-- integers as a dummy Abstract type
main :: IO ()
main = hspec $ do
  describe "Zipper" $ do
    it "fails to go left on a fresh zipper" $
      left (mkZipper ((1 :: Int) :|| 2 :| 3 :| Empty)) `shouldBe` Nothing
    it "goes right on a fresh zipper" $
      right (mkZipper ((1 :: Int) :|| 2 :| 3 :| Empty))
        `shouldBe` Just (Zipper ((1 :: Int) :| Empty) (2 :|| 3 :| Empty))
    it "converts back and forth" $
      (mkZipper . fromZipper . mkZipper $ ((1 :: Int) :|| 2 :| 3 :| Empty))
        `shouldBe` Zipper Empty ((1 :: Int) :|| 2 :| 3 :| Empty)
  describe "iterateOverSequence" $ do
    it "performs a very simple iteration" $ do
      let zipInput = Zipper Empty (Left (0 :: Int16) :|| Empty)
          rules = ((0 :: Int16) :-. (0 :: Int8)) :| Empty
      iterRes <- iterateOverSequence zipInput rules Empty Empty
      snd iterRes `shouldBe` (0 :|| Empty) :| Empty
  describe "iterateOverRules" $ do
    it "performs a very simple iteration" $ do
      let rules = pure $ (0 :: Int16) :-. (0 :: Int8)
      iterRes <- iterateOverRules 0 rules Empty
      iterRes `shouldBe` (Right 0 :|| Empty) :| Empty
  describe "prependRuleMatches" $ do
    it "accepts matches" $ do
      let rule = (0 :: Int16) :-. (0 :: Int8)
      prependation <- prependRuleMatches rule 0 Empty
      prependation `shouldBe` (Right 0 :|| Empty) :| Empty
    it "rejects nonmatches" $ do
      let rule = (0 :: Int16) :-. (0 :: Int8)
      prependation <- prependRuleMatches rule 1 Empty
      prependation `shouldBe` Empty
  describe "generate'" $ do
    it "performs a one-op iteration" $ do
      let rule = pure $ (0 :: Int16) :-. (1 :: Int8)
      res <- generate' rule (pure $ pure $ Left (0 :: Int16)) Empty 1
      res `shouldBe` (pure $ pure 1)
    it "drops strings with lefts" $ do
      let rule1 = (0 :: Int16) :=. ((1 :: Int16) :|| 2 :| Empty)
          rule2 = (1 :-. (3 :: Int8))
          rule3 = (2 :-. 4)
          rules = rule1 :| rule2 :| rule3 :| Empty
      res <- generate' rules (pure $ pure $ Left (0 :: Int16)) Empty 1
      res `shouldBe` Empty
    it "performs a simple single iteration" $ do
      let rule1 = (0 :: Int16) :=. ((1 :: Int16) :|| 2 :| Empty)
          rule2 = (1 :-. (3 :: Int8))
          rule3 = (2 :-. 4)
          rules = rule1 :| rule2 :| rule3 :| Empty
          -- TODO: fix the duplication issue by using sets instead of lists
          expected = (3 :|| 4 :| Empty)
          grammar =
            Grammar
              { grammarRules = rules,
                grammarStart = 0,
                grammarEmptyString = 0
              }
      res <- generate' rules (pure $ pure $ Left (0 :: Int16)) Empty 3
      expected `elem` res `shouldBe` True
      res' <- generate grammar 3
      expected `elem` res' `shouldBe` True
    it "accepts a monadic context of production" $ do
      let rule = (0 :: Int16) :-* (randomIO :: IO Int8)
      let result = generate' (pure rule) (pure $ pure $ Left (0 :: Int16)) Empty 1
      results <- sequence $ replicate 10 $ result
      -- the principle here is that if the numbers were truly
      -- randomly generated then it should be essentially impossible
      -- for ten of them to all to equal each other
      all (== (head results)) results `shouldBe` False
    it "saves fully-Right sequences for future generations" $ do
      let rule = (0 :: Int16) :-* (randomIO :: IO Int8)
          rules = rule :| Empty
      let result = generate' rules (pure $ pure $ Left (0 :: Int16)) Empty 5
      results <- sequence $ replicate 10 $ result
      all (== (head results)) results `shouldBe` False
