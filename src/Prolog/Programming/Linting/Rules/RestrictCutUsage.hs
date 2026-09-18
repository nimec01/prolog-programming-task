module Prolog.Programming.Linting.Rules.RestrictCutUsage (restrictCutUsageRule) where

import Language.Prolog (Clause (..))
import Prolog.Programming.Linting.Helper (termIsCut)
import Prolog.Programming.Linting.Types (Problem (..), ProblemType (RestrictCutUsage), Rule (..))

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
cutExistsInClause (Clause _ rhs) = any termIsCut rhs
cutExistsInClause _ = False

toProblem :: Clause -> Problem
toProblem clause =
  Problem
    { problemType = RestrictCutUsage,
      problemClause = clause,
      problemHint = Just "Don't use the cut (!) operator."
    }
