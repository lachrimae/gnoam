{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gnoam.ContextFree where

import Gnoam.Types

data Rule production nonterminal terminal
  = nonterminal :=. NonNullFinList (Symbol nonterminal terminal)
  | nonterminal :=* production (NonNullFinList (Symbol nonterminal terminal))
  | Schema (nonterminal -> Maybe (production (NonNullFinList (Symbol nonterminal terminal))))

instance (Concrete terminal, Abstract nonterminal, Monad production) => GrammarRule production nonterminal terminal (Rule production nonterminal terminal) where
  getRuleMatch (nonterminal :=. string) symbol =
    if nonterminal == symbol
      then Just $ pure string
      else Nothing
  getRuleMatch (nonterminal :=* mkString) symbol =
    if nonterminal == symbol
      then Just mkString
      else Nothing
  getRuleMatch (Schema f) symbol = f symbol
