module Gnoam.Internal where

import Data.Either
import Data.Functor
import Gnoam.Types

generate ::
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  Grammar production nonterminal terminal ->
  Int ->
  production (FinList (NonNullFinList terminal))
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
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  NonNullFinList (Rule production nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  Int ->
  production (FinList (NonNullFinList terminal))
generate' _ Empty 0 = pure Empty
generate' rules (generation :| generations) 0 =
  if all isRight generation
    then do
      let head' = fmap (\(Right x) -> x) generation
      tail' <- generate' rules generations 0
      pure $ head' :| tail'
    else generate' rules generations 0
generate' rules generations iterations = do
  let generationComputation =
        generations <&> \generation ->
          iterInput (mkZipper generation) rules Empty
  additionalGenerations <- foldl (<>) Empty <$> sequence generationComputation
  let subsequentGenerations = generations <> additionalGenerations
  generate' rules subsequentGenerations (iterations - 1)

iterInput ::
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  Zipper (Either nonterminal terminal) ->
  NonNullFinList (Rule production nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  production (FinList (NonNullFinList (Either nonterminal terminal)))
iterInput zipInput rules generation = do
  substitutions <- iterRules zipInput (mkZipper rules) Empty
  let Zipper prev (_current :|| next) = zipInput
      additionsToGeneration =
        substitutions
          <&> \substitution -> fromZipper $ Zipper prev $ substitution `grow` next
  let nextGeneration = generation <> additionsToGeneration
  case right zipInput of
    Nothing ->
      pure nextGeneration
    Just subsequentZipInput ->
      iterInput
        subsequentZipInput
        rules
        nextGeneration

iterRules ::
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  Zipper (Either nonterminal terminal) ->
  -- TODO
  -- we don't actually use the Zipper features of this var,
  -- it can be made a regular list
  Zipper (Rule production nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  production (FinList (NonNullFinList (Either nonterminal terminal)))
iterRules zipInput zipRules substitutions = do
  let Zipper _ (rule :|| _) = zipRules
      Zipper _ (symbol :|| _) = zipInput
  additionsToSubstitutions <- prependRuleMatches rule symbol substitutions
  let subsequentSubstitutions = substitutions <> additionsToSubstitutions
  case right zipRules of
    Nothing ->
      pure subsequentSubstitutions
    Just subsequentZipRules ->
      iterRules zipInput subsequentZipRules subsequentSubstitutions

prependRuleMatches ::
  (Abstract nonterminal, Concrete terminal, Monad production) =>
  Rule production nonterminal terminal ->
  Either nonterminal terminal ->
  FinList (NonNullFinList ((Either nonterminal terminal))) ->
  production (FinList (NonNullFinList (Either nonterminal terminal)))
prependRuleMatches rule symbol substitutions =
  case symbol of
    Left nonterminal ->
      if (domain rule) nonterminal
        then case codomain rule of
          Left f -> do
            newSubstitution <- f nonterminal
            pure $ fmap Left newSubstitution :| substitutions
          Right f -> do
            terminal <- f nonterminal
            pure $ pure (Right terminal) :| substitutions
        else pure substitutions
    Right _ -> pure substitutions
