{-# LANGUAGE TupleSections #-}

module Prolog.Programming.Linting.Rules.NoUnusedVariables (noUnusedVariablesRule) where

import Data.List (uncons, (\\))
import Data.Maybe (mapMaybe)
import Language.Prolog (Clause (..), Term (..))
import Prolog.Programming.Linting.Helper (namedVariablesInTerm)
import Prolog.Programming.Linting.Types (Problem (..), ProblemType (NoUnusedVariables), Rule (..))

noUnusedVariablesRule :: Rule
noUnusedVariablesRule =
  Rule
    { ruleDetect = detect,
      ruleProblemType = NoUnusedVariables
    }

detect :: [Clause] -> [Problem]
detect predicateDefs = map toProblem clausesWithUnusedVariable
  where
    clausesWithUnusedVariable = mapMaybe (\c -> (c,) <$> clauseHasUnusedVariable c) predicateDefs

clauseHasUnusedVariable :: Clause -> Maybe String
clauseHasUnusedVariable (Clause (Struct _ args) rs) = case uncons args of
  Nothing -> fst <$> uncons (collectUnusedVariables [] rs)
  Just (x, xs) -> case namedVariablesInTerm x of
    [] -> Nothing
    (v : _) -> fst <$> uncons (collectUnusedVariables [v] (xs ++ rs))
clauseHasUnusedVariable _ = Nothing

collectUnusedVariables :: [String] -> [Term] -> [String]
collectUnusedVariables vs [] = vs
collectUnusedVariables vs (t : ts) = collectUnusedVariables ((vs \\ varsInT) ++ (varsInT \\ vs)) ts
  where
    varsInT = namedVariablesInTerm t

toProblem :: (Clause, String) -> Problem
toProblem (clause, unused) =
  Problem
    { problemType = NoUnusedVariables,
      problemClause = clause,
      problemHint = Just $ "Replace " ++ unused ++ " with wildcard (_) ."
    }
