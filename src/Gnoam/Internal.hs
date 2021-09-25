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
                :| grammarRules rawGrammar
          }
   in generate'
        (grammarRules grammar)
        (pure $ pure (Left (grammarStart grammar)))
        Empty
        iterations

generate' ::
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  FinList (Rule production nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  FinList (NonNullFinList terminal) ->
  Int ->
  production (FinList (NonNullFinList terminal))
generate' _ _ fullyTerminalSeqs 0 = pure fullyTerminalSeqs
generate' rules seqsWithNonterminals fullyTerminalSeqs iterationsLeft = do
  let newSeqs =
        seqsWithNonterminals <&> \seqWithNonterminals ->
          iterateOverSequence (mkZipper seqWithNonterminals) rules Empty Empty
  (newSeqsWithNonterminals, newFullyTerminalSeqs) <-
    foldl accumResults (Empty, Empty) <$> sequence newSeqs
  generate' rules newSeqsWithNonterminals (fullyTerminalSeqs <> newFullyTerminalSeqs) (iterationsLeft - 1)
  where
    accumResults (oldGenerations, oldTerminals) (newGenerations, newTerminals) =
      (oldGenerations <> newGenerations, oldTerminals <> newTerminals)

iterateOverSequence ::
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  Zipper (Either nonterminal terminal) ->
  FinList (Rule production nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  FinList (NonNullFinList terminal) ->
  production (FinList (NonNullFinList (Either nonterminal terminal)), FinList (NonNullFinList terminal))
iterateOverSequence zipper@(Zipper _ (Right _ :|| _)) rules newSequences newFullyTerminalSequences =
  case right zipper of
    Nothing -> pure (newSequences, newFullyTerminalSequences)
    Just nextZip -> iterateOverSequence nextZip rules newSequences newFullyTerminalSequences
iterateOverSequence zipper@(Zipper prev (Left current :|| next)) rules newSequences newFullyTerminalSequences = do
  substitutions <- iterateOverRules current rules Empty
  let allNewSeqs =
        substitutions
          <&> \substitution -> fromZipper $ Zipper prev $ substitution `grow` next
      (newSequences', newFullyTerminalSequences') = separateFullyTerminalSequences allNewSeqs (Empty, Empty)
      agglomeratedSequences = newSequences <> newSequences'
      agglomeratedFullyTerminalSequences = newFullyTerminalSequences <> newFullyTerminalSequences'
  case right zipper of
    Nothing -> pure (agglomeratedSequences, agglomeratedFullyTerminalSequences)
    Just nextZipper -> iterateOverSequence nextZipper rules agglomeratedSequences agglomeratedFullyTerminalSequences
  where
    separateFullyTerminalSequences Empty pair = pair
    separateFullyTerminalSequences (sequence' :| sequences) (nonFullyTerminal, fullyTerminal) =
      case identifySequence sequence' of
        Right aFullyTerminalSeq -> separateFullyTerminalSequences sequences (nonFullyTerminal, aFullyTerminalSeq :| fullyTerminal)
        Left aNonFullyTerminalSeq -> separateFullyTerminalSequences sequences (aNonFullyTerminalSeq :| nonFullyTerminal, fullyTerminal)
    identifySequence sequence' =
      if all isRight sequence'
        then Right $ fmap (\(Right x) -> x) sequence'
        else Left sequence'

iterateOverRules ::
  (Concrete terminal, Abstract nonterminal, Monad production) =>
  nonterminal ->
  FinList (Rule production nonterminal terminal) ->
  FinList (NonNullFinList (Either nonterminal terminal)) ->
  production (FinList (NonNullFinList (Either nonterminal terminal)))
iterateOverRules _ Empty substitutions = pure substitutions
iterateOverRules symbol (rule :| rules) substitutions = do
  additionalSubstitution <- prependRuleMatches rule symbol
  let agglomeratedSubstitutions = case additionalSubstitution of
        Nothing -> substitutions
        Just newSubstitution -> newSubstitution :| substitutions
  iterateOverRules symbol rules agglomeratedSubstitutions

prependRuleMatches ::
  (Abstract nonterminal, Concrete terminal, Monad production) =>
  Rule production nonterminal terminal ->
  nonterminal ->
  production (Maybe (NonNullFinList (Either nonterminal terminal)))
prependRuleMatches rule symbol =
  if (domain rule) symbol
    then case codomain rule of
      Left f -> do
        newSubstitution <- f symbol
        pure $ Just $ fmap Left newSubstitution
      Right f -> do
        terminal <- f symbol
        pure $ Just $ (Right terminal :|| Empty)
    else pure Nothing
