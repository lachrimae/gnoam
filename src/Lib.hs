{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Data.Either
import Data.Foldable
import Data.Functor

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

-- An equivalence class on `a`. The user must ensure
-- that `a == b` implies that `a ~~ b`.
class Equiv a where
  (~~) :: a -> a -> Bool

-- we need terminal symbols of our language to be Concrete
type Concrete a = (Eq a, Ord a, Bounded a, Enum a, Show a)
type Abstract a = (Eq a, Equiv a, Ord a, Show a)

data (Abstract nonterminal, Concrete terminal) => Rule nonterminal terminal
  = -- production of a pair of nonterminals
    !nonterminal :=. !(nonterminal, nonterminal)
  | -- production of a terminal
    !nonterminal :-. !terminal
  | -- production schema of a pair of nonterminals
    -- this production rule specifies an arbitrary
    -- number of rules sharing its form
    !nonterminal :=> !(nonterminal -> (nonterminal, nonterminal))
  | -- production schema of a terminal
    -- ditto
    !nonterminal :-> !(nonterminal -> terminal)

instance (Abstract nonterminal, Concrete terminal) => Show (Rule nonterminal terminal) where
  show (a :=. b) = show a <> " :=. " <> show b
  show (a :-. b) = show a <> " :-. " <> show b
  show (a :=> _) = show a <> " :=> [schema of substitution to nonterminals `BC`]"
  show (a :-> _) = show a <> " :=> [schema of substitution to terminals `a`]"

domain :: (Abstract nonterminal, Concrete terminal) => Rule nonterminal terminal -> nonterminal
domain (a :=. _) = a
domain (a :-. _) = a
domain (a :=> _) = a
domain (a :-> _) = a

codomain :: (Abstract nonterminal, Concrete terminal) => Rule nonterminal terminal -> Either (nonterminal -> (nonterminal, nonterminal)) (nonterminal -> terminal)
codomain (_ :=. (a, b)) = Left $ const (a, b)
codomain (_ :-. a) = Right $ const a
codomain (_ :=> f) = Left f
codomain (_ :-> f) = Right f

data Grammar nonterminal terminal
  = Grammar
      { grammarRules :: !(NonNullFinList (Rule nonterminal terminal))
      , grammarStart :: !nonterminal
      , grammarEmptyString :: !terminal
      }
  deriving (Show)

generate
  :: (Concrete terminal, Abstract nonterminal)
  => Grammar nonterminal terminal
  -> Int
  -> FinList (NonNullFinList terminal)
generate rawGrammar iterations =
  -- we add the empty string to the language output if it is not already a rule
  -- as per the definition of a context-free language
  -- in the worst case scenario, this adds an duplicated (start -> emptyString)
  -- rule to the program, which is not a big deal once I
  -- switch this to using sets
  let grammar =
        rawGrammar
          { grammarRules =
            (grammarStart rawGrammar :-. grammarEmptyString rawGrammar)
              `prepend` grammarRules rawGrammar
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
            Left f ->
              let (nonterminal1, nonterminal2) = f nonterminal
               in ((Left nonterminal1 :|| Left nonterminal2 :| Empty)) :| matches
            Right f ->
              let terminal = f nonterminal
               in pure (Right terminal) :| matches
          else matches
      Right _ -> matches
