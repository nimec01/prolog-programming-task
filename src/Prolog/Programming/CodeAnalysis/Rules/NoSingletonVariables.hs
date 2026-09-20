{-# LANGUAGE TupleSections #-}

module Prolog.Programming.CodeAnalysis.Rules.NoSingletonVariables (noSingletonVariablesRule) where

import Data.List (uncons, (\\))
import Data.Maybe (mapMaybe)
import Language.Prolog (Clause (..), Term (..))
import Prolog.Programming.CodeAnalysis.Helper (namedVariablesInTerm)
import Prolog.Programming.CodeAnalysis.Types (Problem (..), ProblemType (NoSingletonVariables), Rule (..))

noSingletonVariablesRule :: Rule
noSingletonVariablesRule =
  Rule
    { ruleDetect = detect,
      ruleProblemType = NoSingletonVariables
    }

detect :: [Clause] -> [Problem]
detect predicateDefs = map toProblem clausesWithSingletonVariable
  where
    clausesWithSingletonVariable = mapMaybe (\c -> (c,) <$> clauseHasSingletonVariable c) predicateDefs

clauseHasSingletonVariable :: Clause -> Maybe String
clauseHasSingletonVariable (Clause (Struct _ args) rs) = case uncons args of
  Nothing -> fst <$> uncons (collectSingletonVariables [] rs)
  Just (x, xs) -> case namedVariablesInTerm x of
    [] -> Nothing
    (v : _) -> fst <$> uncons (collectSingletonVariables [v] (xs ++ rs))
clauseHasSingletonVariable _ = Nothing

collectSingletonVariables :: [String] -> [Term] -> [String]
collectSingletonVariables vs [] = vs
collectSingletonVariables vs (t : ts) = collectSingletonVariables ((vs \\ varsInT) ++ (varsInT \\ vs)) ts
  where
    varsInT = namedVariablesInTerm t

toProblem :: (Clause, String) -> Problem
toProblem (clause, var) =
  Problem
    { problemType = NoSingletonVariables,
      problemClause = clause,
      problemHint = Just $ "Replace " ++ var ++ " with wildcard (_) ."
    }
