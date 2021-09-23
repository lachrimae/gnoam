{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Gnoam.Internal where

import Data.Either
import Data.Foldable
import Data.Functor

generate ::
  (Concrete terminal, Abstract nonterminal) =>
  Grammar nonterminal terminal ->
  Int ->
  FinList (NonNullFinList terminal)
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

generate' ::
  (Concrete terminal, Abstract nonterminal) =>
  NonNullFinList (Rule nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  Int ->
  FinList (NonNullFinList terminal)
generate' _ Empty 0 = Empty
generate' rules (generation :| generations) 0 =
  if all isRight generation
    then fmap (\(Right x) -> x) generation :| (generate' rules generations 0)
    else generate' rules generations 0
generate' rules generations iterations =
  let mappedGenerations =
        generations <&> \generation ->
          iterInput right (mkZipper generation) rules Empty
      concatGenerations = foldl' (<>) Empty mappedGenerations
   in generate' rules concatGenerations (iterations - 1)

iterInput ::
  (Concrete terminal, Abstract nonterminal) =>
  (Zipper (Either nonterminal terminal) -> Maybe (Zipper (Either nonterminal terminal))) ->
  Zipper (Either nonterminal terminal) ->
  NonNullFinList (Rule nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  FinList (NonNullFinList (Either nonterminal terminal))
iterInput motion zipInput rules generation =
  let substitutions = iterRules right zipInput (mkZipper rules) Empty
      Zipper prev (_current :|| next) = zipInput
      additionsToGeneration =
        substitutions
          <&> \substitution -> fromZipper $ Zipper prev $ substitution `grow` next
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

iterRules ::
  (Concrete terminal, Abstract nonterminal) =>
  (Zipper (Rule nonterminal terminal) -> Maybe (Zipper (Rule nonterminal terminal))) ->
  Zipper (Either nonterminal terminal) ->
  Zipper (Rule nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  FinList (NonNullFinList (Either nonterminal terminal))
iterRules motion zipInput zipRules substitutions =
  let Zipper _ (rule :|| _) = zipRules
      Zipper _ (symbol :|| _) = zipInput
      subsequentSubstitutions = prependRuleMatches rule symbol substitutions
   in case motion zipRules of
        Nothing ->
          subsequentSubstitutions
        Just subsequentZipRules ->
          iterRules motion zipInput subsequentZipRules subsequentSubstitutions

prependRuleMatches ::
  (Abstract nonterminal, Concrete terminal) =>
  Rule nonterminal terminal ->
  Either nonterminal terminal ->
  FinList (NonNullFinList ((Either nonterminal terminal))) ->
  FinList (NonNullFinList (Either nonterminal terminal))
prependRuleMatches rule symbol matches =
  case symbol of
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
