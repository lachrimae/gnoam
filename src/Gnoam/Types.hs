{-# LANGUAGE MultiParamTypeClasses #-}

module Gnoam.Types where

-- strictness forces this list to be finite
data FinList a
  = a :| !(FinList a)
  | Empty
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup (FinList a) where
  (x :| xs) <> ys = x :| (xs <> ys)
  Empty <> ys = ys

instance Monoid (FinList a) where
  mappend = (<>)
  mempty = Empty

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

infixr 6 :||

data Zipper a = Zipper
  { previous :: !(FinList a),
    subsequent :: !(NonNullFinList a)
  }
  deriving (Show, Eq)

mkZipper :: NonNullFinList a -> Zipper a
mkZipper l = Zipper Empty l

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ (_ :|| Empty)) = Nothing
right (Zipper as (b :|| b' :| bs)) = Just $ Zipper (b :| as) (b' :|| bs)

left :: Zipper a -> Maybe (Zipper a)
left (Zipper Empty _) = Nothing
left (Zipper (a :| as) (b :|| bs)) = Just $ Zipper as (a :|| b :| bs)

fromZipper :: Zipper a -> NonNullFinList a
fromZipper zipper =
  case left zipper of
    Nothing -> subsequent zipper
    Just z -> fromZipper z

type Concrete a = (Eq a, Ord a, Show a)

type Abstract a = (Eq a, Ord a, Show a)

data Symbol nonterminal terminal
  = NonTerminal nonterminal
  | Terminal terminal

isTerminal :: Symbol nonterminal terminal -> Bool
isTerminal (Terminal _) = True
isTerminal (NonTerminal _) = False

class (Concrete terminal, Abstract nonterminal, Monad production) => GrammarRule production nonterminal terminal rule where
  getRuleMatch ::
    rule ->
    nonterminal ->
    Maybe (production (NonNullFinList (Symbol nonterminal terminal)))
