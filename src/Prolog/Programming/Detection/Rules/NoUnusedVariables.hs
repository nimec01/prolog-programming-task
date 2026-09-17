{-# LANGUAGE TupleSections #-}

module Prolog.Programming.Detection.Rules.NoUnusedVariables (noUnusedVariables) where

import Data.List (find, intersect)
import Data.Maybe (mapMaybe)
import Language.Prolog (Clause (..), Term (..))
import Prolog.Programming.Detection.Helper (namedVariablesInTerm)
import Prolog.Programming.Detection.Types (Problem (..), ProblemType (NoUnusedVariables), Rule (..))

noUnusedVariables :: Rule
noUnusedVariables =
  Rule
    { ruleDetect = detect,
      ruleProblemType = NoUnusedVariables
    }

detect :: [Clause] -> [Problem]
detect predicateDefs = map toProblem clausesWithUnusedVariable
  where
    clausesWithUnusedVariable = mapMaybe (\c -> (c,) <$> clauseHasUnusedVariable c) predicateDefs

clauseHasUnusedVariable :: Clause -> Maybe String
clauseHasUnusedVariable (Clause (Struct _ args) rs) = fst <$> find (\(_, xs) -> null xs) leftVarsUsage
  where
    leftVars = concatMap namedVariablesInTerm args
    leftVarsUsage = map (\v -> (v, filter (termUsesVariables [v]) rs)) leftVars
clauseHasUnusedVariable _ = Nothing

termUsesVariables :: [String] -> Term -> Bool
termUsesVariables vs term = not $ null $ vs `intersect` namedVariablesInTerm term

toProblem :: (Clause, String) -> Problem
toProblem (clause, unused) =
  Problem
    { problemType = NoUnusedVariables,
      problemClause = clause,
      problemHint = Just $ "Replace " ++ unused ++ " with wildcard (_) ."
    }
