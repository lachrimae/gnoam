{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

-- strictness forces this list to be finite
data FinList a
  = a :| !(FinList a) 
  | Empty
  deriving stock (Show, Eq, Ord, Foldable)

instance Functor FinList where
  fmap _ Empty = Empty
  fmap f (a :| as) = f a :| fmap f as

data Zipper a
  = Zipper
      { previous :: FinList a
      , next :: FinList a
      }

mkZipper :: FinList a -> Zipper a
mkZipper l = Zipper Empty l

left :: Zipper a -> Maybe (Zipper a)
left (Zipper Empty _) = Nothing
left (Zipper (a :| as) bs) = Just $ Zipper as (a :| bs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ Empty) = Nothing
right (Zipper as (b :| bs)) = Just $ Zipper (b :| as) bs

fromZipper :: Zipper a -> FinList a
fromZipper zipper =
 case left zipper of
   Nothing -> next zipper
   Just z -> fromZipper z

-- we need terminal symbols of our language to be Concrete
type Concrete a = (Eq a, Ord a, Bounded a, Enum a)

-- Nonterminal symbols will be Abstract
data Eq a => Abstract a
  = Start
  | Term a
  deriving stock (Show, Eq, Ord)

-- Nothing represents the empty string, and is always a valid terminating symbol
data (Concrete a, Ord b) => Alphabet a b
  = Terminal a
  | NonTerminal (Abstract b)
  deriving stock (Eq, Ord)

data (Concrete a, Ord b) => Expression a b
  = Expression (FinList (Alphabet a b)) (Alphabet a b) (FinList (Alphabet a b))
  deriving stock (Eq, Ord)

-- A relation may be expressed as a set of tuples (a, b)
-- Rule is essentially a tuple, and Relation is roughly a set of tuples.
data Rule a b = a :-> b

preimage :: Rule a b -> a
preimage (a :-> _) = a

image :: Rule a b -> b
image (_ :-> b) = b

newtype Relation a b = Relation (FinList (Rule a b))

-- these might want to be newtypes
type Production a b = (Concrete a, Ord b) => Rule (Expression a b) (Expression a b)
type Grammar a b = (Concrete a, Ord b) => Relation (Expression a b) (Expression a b)

-- Start -> s
unitGrammar :: (Eq a, Ord b) => Grammar a b
unitGrammar = 
  let relationElt =  Expression Empty (NonTerminal Start) Empty :-> Expression Empty (Terminal minBound) Empty
   in Relation $ relationElt :| Empty

generate :: (Concrete a, Ord b) => FinList (Alphabet a b) -> Grammar a b -> Either (FinList (FinList ((Alphabet a b)))) (FinList (FinList a))
generate input grammar =
  -- should be requiredStartahead instead of requiredLookbehind because we iterate from the start of lists
  let requiredLookbehind = maxLength (\(Expression behind _ _) -> behind) grammar
      requiredLookahead = maxLength (\(Expression _ _ ahead) -> ahead) grammar
   in undefined
  where
    maxLength :: (Concrete a, Ord b) => (Expression a b -> FinList (Alphabet a b)) -> Grammar a b -> Int
    maxLength lookDirection (Relation rules)
      = maximum $ fmap (length . lookDirection . preimage) rules
