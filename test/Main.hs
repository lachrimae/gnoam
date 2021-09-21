module Main where
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
