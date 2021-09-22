{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Data.Either
import Data.Foldable
import Data.Functor
import Data.Maybe
import Debug.Trace

-- strictness forces this list to be finite
data FinList a
  = a :| !(FinList a)
  | Empty
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup (FinList a) where
  (x :| xs) <> ys = x :| (xs <> ys)
  Empty <> ys = ys

instance Applicative FinList where
  pure x = x :| Empty
  fs@(f :| fss) <*> (x :| xs) = (f x :| (fs <*> xs)) <> (fss <*> xs)
  _ <*> Empty = Empty
  Empty <*> _ = Empty

data NonNullFinList a
  = a :|| !(FinList a)
  deriving stock (Show, Eq, Ord, Functor, Foldable)

fromNonNull :: NonNullFinList a -> FinList a
fromNonNull (x :|| xs) = x :| xs

instance Semigroup (NonNullFinList a) where
  (x :|| xs) <> ys = x :|| (xs <> fromNonNull ys)

instance Applicative NonNullFinList where
  pure x = x :|| Empty
  fs@(f :|| fss) <*> (x :|| xss) = f x :|| ((fromNonNull fs <*> xss) <> (fss <*> xss))

prepend :: a -> NonNullFinList a -> NonNullFinList a
prepend x (y :|| y' :| ys) = x :|| y :| y' :| ys
prepend x (y :|| Empty) = x :|| y :| Empty

grow :: NonNullFinList a -> FinList a -> NonNullFinList a
grow (x :|| xs) ys = x :|| (xs <> ys)

infixr 7 :|
infixr 7 :||

data Zipper a = Zipper
  { previous :: !(FinList a),
    subsequent :: !(NonNullFinList a)
  }
  deriving (Show, Eq)

mkZipper :: NonNullFinList a -> Zipper a
mkZipper l = Zipper Empty l

left :: Zipper a -> Maybe (Zipper a)
left (Zipper Empty _) = Nothing
left (Zipper (a :| as) bs) = Just $ Zipper as (prepend a bs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ (_ :|| Empty)) = Nothing
right (Zipper as (b :|| b' :| bs)) = Just $ Zipper (b :| as) (b' :|| bs)

fromZipper :: Zipper a -> NonNullFinList a
fromZipper zipper =
  case left zipper of
    Nothing -> subsequent zipper
    Just z -> fromZipper z

-- we need terminal symbols of our language to be Concrete
type Concrete a = (Eq a, Ord a, Bounded a, Enum a, Show a)
type Abstract a = (Eq a, Ord a, Show a)

data (Abstract nonterminal, Concrete terminal) => Rule nonterminal terminal
  = !nonterminal := !(nonterminal, nonterminal)
  | !nonterminal :- !terminal
  deriving (Show, Eq, Ord)

domain :: (Abstract nonterminal, Concrete terminal) => Rule nonterminal terminal -> nonterminal
domain (a := _) = a
domain (a :- _) = a

codomain :: (Abstract nonterminal, Concrete terminal) => Rule nonterminal terminal -> Either (nonterminal, nonterminal) terminal
codomain (_ := (a, b)) = Left (a, b)
codomain (_ :- a) = Right a

data Grammar nonterminal terminal
  = Grammar
      { grammarRules :: !(NonNullFinList (Rule nonterminal terminal))
      , grammarStart :: !nonterminal
      , grammarEmptyString :: !terminal
      }
  deriving (Show, Eq)

generate
  :: (Concrete terminal, Abstract nonterminal)
  => Grammar nonterminal terminal
  -> Int
  -> FinList (NonNullFinList terminal)
generate rawGrammar iterations =
  -- we add the empty string to the language output if it is not already a rule
  -- as per the definition of a context-free language
  let grammar =
        if isJust $
             find (== (grammarStart rawGrammar :- grammarEmptyString rawGrammar))
             (grammarRules rawGrammar)
          then rawGrammar
          else
            rawGrammar
              { grammarRules =
                  (grammarStart rawGrammar :- grammarEmptyString rawGrammar)
                    `prepend` grammarRules grammar
              }
   in generate'
        (grammarRules grammar)
        (pure $ pure (Left (grammarStart grammar)))
        iterations

generate'
  :: (Concrete terminal, Abstract nonterminal)
  => NonNullFinList (Rule nonterminal terminal)
  -> FinList (NonNullFinList (Either nonterminal terminal))
  -> Int
  -> FinList (NonNullFinList terminal)
generate' _ Empty 0 = Empty
generate' rules (generation :| generations) 0 =
  if all isRight generation
    then fmap (\(Right x) -> x) generation :| (generate' rules generations 0)
    else generate' rules generations 0
generate' rules generations iterations =
  let mappedGenerations = generations <&> \generation ->
        iterInput right (mkZipper generation) rules Empty
      concatGenerations = foldl' (<>) Empty mappedGenerations
      debug = show rules <> " " <> show generations <> " " <> show iterations
   in generate' rules concatGenerations (iterations - 1)

iterInput
  :: (Concrete terminal, Abstract nonterminal)
  => (Zipper (Either nonterminal terminal) -> Maybe (Zipper (Either nonterminal terminal)))
  -> Zipper (Either nonterminal terminal)
  -> NonNullFinList (Rule nonterminal terminal)
  -> FinList (NonNullFinList (Either nonterminal terminal))
  -> FinList (NonNullFinList (Either nonterminal terminal))
iterInput motion zipInput rules generation =
  let substitutions = iterRules right zipInput (mkZipper rules) Empty
      Zipper prev (_current :|| next) = zipInput
      additionsToGeneration =
        substitutions <&>
          \substitution -> fromZipper $ Zipper prev $ substitution `grow` next
      nextGeneration = generation <> additionsToGeneration
   in case motion zipInput of
        Nothing ->
          nextGeneration
        Just subsequentZipInput ->
          iterInput
            motion
            subsequentZipInput
            rules
            nextGeneration

iterRules
  :: (Concrete terminal, Abstract nonterminal)
  => (Zipper (Rule nonterminal terminal) -> Maybe (Zipper (Rule nonterminal terminal)))
  -> Zipper (Either nonterminal terminal)
  -> Zipper (Rule nonterminal terminal)
  -> FinList (NonNullFinList (Either nonterminal terminal))
  -> FinList (NonNullFinList (Either nonterminal terminal))
iterRules motion zipInput zipRules substitutions =
  let Zipper _ (rule :|| _) = zipRules
      Zipper _ (symbol :|| _) = zipInput
      subsequentSubstitutions = prependRuleMatches rule symbol substitutions
   in case motion zipRules of
        Nothing ->
          subsequentSubstitutions
        Just subsequentZipRules ->
          iterRules motion zipInput subsequentZipRules subsequentSubstitutions

prependRuleMatches
  :: (Abstract nonterminal, Concrete terminal)
  => Rule nonterminal terminal
  -> Either nonterminal terminal
  -> FinList (NonNullFinList ((Either nonterminal terminal)))
  -> FinList (NonNullFinList (Either nonterminal terminal))
prependRuleMatches rule symbol matches
  = case symbol of
      Left nonterminal ->
        if domain rule == nonterminal
          then case codomain rule of
            Left (nonterminal1, nonterminal2) ->
              ((Left nonterminal1 :|| Left nonterminal2 :| Empty)) :| matches
            Right terminal ->
              pure (Right terminal) :| matches
          else matches
      Right _ -> matches
