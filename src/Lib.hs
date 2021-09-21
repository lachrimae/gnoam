{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Data.Foldable
import Data.Maybe

-- strictness forces this list to be finite
data FinList a
  = a :| !(FinList a)
  | Empty
  deriving stock (Show, Eq, Ord, Foldable)

data NonNullFinList a
  = a :|| !(FinList a)
  deriving stock (Show, Eq, Ord, Foldable)

infixr 7 :|
infixr 7 :||

instance Functor FinList where
  fmap _ Empty = Empty
  fmap f (a :| as) = f a :| fmap f as

data Zipper a = Zipper
  { previous :: !(FinList a),
    next :: !(NonNullFinList a)
  }
  deriving (Show, Eq)

mkZipper :: NonNullFinList a -> Zipper a
mkZipper l = Zipper Empty l

left :: Zipper a -> Maybe (Zipper a)
left (Zipper Empty _) = Nothing
left (Zipper (a :| as) (b :|| bs)) = Just $ Zipper as (a :|| b :| bs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ (_ :|| Empty)) = Nothing
right (Zipper as (b :|| b' :| bs)) = Just $ Zipper (b :| as) (b' :|| bs)

fromZipper :: Zipper a -> NonNullFinList a
fromZipper zipper =
  case left zipper of
    Nothing -> next zipper
    Just z -> fromZipper z

-- we need terminal symbols of our language to be Concrete
type Concrete a = (Eq a, Ord a, Bounded a, Enum a)

data (Ord nonterminal, Concrete terminal) => Rule nonterminal terminal
  = !nonterminal := !(nonterminal, nonterminal)
  | !nonterminal :- !terminal
  deriving (Show, Eq, Ord)

data Grammar terminal nonterminal
  = Grammar
      { rules :: !(NonNullFinList (Rule nonterminal terminal))
      , start :: !nonterminal
      , emptyString :: !terminal
      }
  deriving (Show, Eq)

generate :: (Concrete terminal, Ord nonterminal) => NonNullFinList (Either terminal nonterminal) -> Grammar terminal nonterminal -> NonNullFinList (NonNullFinList terminal)
generate input rawGrammar =
  let grammar =
        if isJust $ find (== (start rawGrammar :- emptyString rawGrammar)) (rules rawGrammar)
          then grammar
          else
            let firstRule :|| otherRules = rules rawGrammar
             in grammar
                  { rules = (start rawGrammar :- emptyString rawGrammar) :|| (firstRule :| otherRules)
                  }
      input' = mkZipper input
      rules' = mkZipper . rules $ grammar
      innerIter zipState rulesState = undefined
   in undefined
