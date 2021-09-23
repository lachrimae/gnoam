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

{- ORMOLU_DISABLE -}
data (Abstract nonterminal, Concrete terminal, Monad production) => Rule production nonterminal terminal
  = !nonterminal :=. !(NonNullFinList nonterminal)
  | !nonterminal :-. !(terminal)
  | !nonterminal :=* !(production (NonNullFinList nonterminal))
  | !nonterminal :-* !(production terminal)
  | !(nonterminal -> Bool) :=> !(nonterminal -> production (NonNullFinList nonterminal))
  | !(nonterminal -> Bool) :-> !(nonterminal -> production terminal)
{- ORMOLU_ENABLE -}

instance (Abstract nonterminal, Concrete terminal, Monad m) => Show (Rule m nonterminal terminal) where
  show (a :=. b) = show a <> " :=. " <> show b
  show (a :-. b) = show a <> " :-. " <> show b
  show (a :=* _) = show a <> " :=* [some schema of substitution by nonterminals `BC`]"
  show (a :-* _) = show a <> " :-* [some schema of substitution by terminals `a`]"
  show (_ :=> _) = "[some predicate on nonterminals `A`] :=> [some schema of substitution by nonterminals `BC`]"
  show (_ :-> _) = "[some predicate on nonterminals `A`] :=> [some schema of substitution by terminals `a`]"

domain :: (Abstract nonterminal, Concrete terminal, Monad m) => Rule m nonterminal terminal -> (nonterminal -> Bool)
domain (a :=. _) = (== a)
domain (a :-. _) = (== a)
domain (a :=* _) = (== a)
domain (a :-* _) = (== a)
domain (p :=> _) = p
domain (p :-> _) = p

codomain :: (Abstract nonterminal, Concrete terminal, Monad production) => Rule production nonterminal terminal -> Either (nonterminal -> production (NonNullFinList nonterminal)) (nonterminal -> production terminal)
codomain (_ :=. as) = Left . const . pure $ as
codomain (_ :-. a) = Right . const . pure $ a
codomain (_ :=* z) = Left $ const $ z
codomain (_ :-* z) = Right $ const $ z
codomain (_ :=> f) = Left f
codomain (_ :-> f) = Right f

data Grammar m nonterminal terminal = Grammar
  { grammarRules :: !(NonNullFinList (Rule m nonterminal terminal)),
    grammarStart :: !nonterminal,
    grammarEmptyString :: !terminal
  }
  deriving (Show)
