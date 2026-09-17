module Prolog.Programming.Detection.Rules.RestrictCutUsage (restrictCutUsageRule) where

import Language.Prolog (Clause (..))
import Prolog.Programming.Detection.Helper (termIsCut)
import Prolog.Programming.Detection.Types (Problem (..), ProblemType (RestrictCutUsage), Rule (..))

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
