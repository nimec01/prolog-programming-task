module Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule) where

import Language.Prolog (Clause (..))
import Prolog.Programming.CodeAnalysis.Helper (termContainsCut)
import Prolog.Programming.CodeAnalysis.Types (Problem (..), ProblemType (RestrictCutUsage), Rule (..))

restrictCutUsageRule :: Rule
restrictCutUsageRule =
  Rule
    { ruleDetect = detect,
      ruleProblemType = RestrictCutUsage
    }

detect :: [Clause] -> [Problem]
detect clauses = map toProblem clausesWithCuts
  where
    clausesWithCuts = filter cutExistsInClause clauses

cutExistsInClause :: Clause -> Bool
cutExistsInClause (Clause _ rhs) = any termContainsCut rhs
cutExistsInClause _ = False

toProblem :: Clause -> Problem
toProblem clause =
  Problem
    { problemType = RestrictCutUsage,
      problemClause = clause,
      problemHint = Just "Don't use the cut (!) operator."
    }
