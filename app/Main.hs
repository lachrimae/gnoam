module Main where

import Data.Int
import Gnoam

main :: IO ()
main = do
  let grammar =
        Grammar
          { grammarRules =
              ((0 :: Int16) :- (1 :: Int8))
                :|| Empty,
            grammarStart = 0,
            grammarEmptyString = 0
          }
  putStrLn $ show $ generate grammar 4
