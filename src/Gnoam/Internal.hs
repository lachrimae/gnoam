module Gnoam.Internal where

import Data.Functor
import Gnoam.Types

generate' ::
  GrammarRule production nonterminal terminal rule =>
  FinList rule ->
  FinList (NonNullFinList (Symbol nonterminal terminal)) ->
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
  GrammarRule production nonterminal terminal rule =>
  Zipper (Symbol nonterminal terminal) ->
  FinList rule ->
  FinList (NonNullFinList (Symbol nonterminal terminal)) ->
  FinList (NonNullFinList terminal) ->
  production (FinList (NonNullFinList (Symbol nonterminal terminal)), FinList (NonNullFinList terminal))
iterateOverSequence zipper@(Zipper _ (Terminal _ :|| _)) rules newSequences newFullyTerminalSequences =
  case right zipper of
    Nothing -> pure (newSequences, newFullyTerminalSequences)
    Just nextZip -> iterateOverSequence nextZip rules newSequences newFullyTerminalSequences
iterateOverSequence zipper@(Zipper prev (NonTerminal current :|| next)) rules newSequences newFullyTerminalSequences = do
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
        Terminal aFullyTerminalSeq -> separateFullyTerminalSequences sequences (nonFullyTerminal, aFullyTerminalSeq :| fullyTerminal)
        NonTerminal aNonFullyTerminalSeq -> separateFullyTerminalSequences sequences (aNonFullyTerminalSeq :| nonFullyTerminal, fullyTerminal)
    identifySequence sequence' =
      if all isTerminal sequence'
        then Terminal $ fmap (\(Terminal x) -> x) sequence'
        else NonTerminal sequence'

iterateOverRules ::
  GrammarRule production nonterminal terminal rule =>
  nonterminal ->
  FinList rule ->
  FinList (NonNullFinList (Symbol nonterminal terminal)) ->
  production (FinList (NonNullFinList (Symbol nonterminal terminal)))
iterateOverRules _ Empty substitutions = pure substitutions
iterateOverRules symbol (rule :| rules) substitutions = do
  additionalSubstitution <- prependRuleMatches rule symbol
  let agglomeratedSubstitutions = case additionalSubstitution of
        Nothing -> substitutions
        Just newSubstitution -> newSubstitution :| substitutions
  iterateOverRules symbol rules agglomeratedSubstitutions

prependRuleMatches ::
  GrammarRule production nonterminal terminal rule =>
  rule ->
  nonterminal ->
  production (Maybe (NonNullFinList (Symbol nonterminal terminal)))
prependRuleMatches rule symbol = do
  case getRuleMatch rule symbol of
    Nothing -> pure Nothing
    Just makeList -> Just <$> makeList
