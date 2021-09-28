{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gnoam.RightRegular where

import Gnoam.Types

data Rule production nonterminal terminal
  = nonterminal :-. NonNullFinList terminal
  | nonterminal :=. (FinList terminal, terminal)
  | nonterminal :-* production (NonNullFinList terminal)
  | nonterminal :=* production (FinList terminal, nonterminal)
  | Schema (nonterminal -> Maybe (production (FinList terminal, nonterminal)))

instance (Concrete terminal, Abstract nonterminal, Monad production) => GrammarRule production nonterminal terminal (Rule production nonterminal terminal) where
  getRuleMatch (nonterminal :-. string) symbol =
    if nonterminal == symbol
      then Just . pure . fmap Terminal $ string
      else Nothing
  getRuleMatch (nonterminal :-* mkString) symbol =
    if nonterminal == symbol
      then Just . fmap (fmap Terminal) $ mkString
      else Nothing
