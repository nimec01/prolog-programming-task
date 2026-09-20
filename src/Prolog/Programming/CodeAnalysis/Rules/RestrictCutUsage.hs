{-# LANGUAGE OverloadedStrings #-}

module Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule) where

import Data.Text.Lazy (pack)
import Language.Prolog (Clause (..))
import Prolog.Programming.CodeAnalysis.Helper (termContainsCut)
import Prolog.Programming.CodeAnalysis.Types (Problem (..), ProblemType (RestrictCutUsage), Rule (..), Severity)
import Text.PrettyPrint.Leijen.Text (brackets, hsep, indent, linebreak, string, vsep)

restrictCutUsageRule :: Rule
restrictCutUsageRule =
  Rule
    { ruleDetect = detect,
      ruleProblemType = RestrictCutUsage
    }

detect :: Severity -> [Clause] -> [Problem]
detect sev clauses = map (toProblem sev) clausesWithCuts
  where
    clausesWithCuts = filter cutExistsInClause clauses

cutExistsInClause :: Clause -> Bool
cutExistsInClause (Clause _ rhs) = any termContainsCut rhs
cutExistsInClause _ = False

toProblem :: Severity -> Clause -> Problem
toProblem sev clause =
  Problem
    { problemType = RestrictCutUsage,
      problemClause = clause,
      problemSeverity = sev,
      problemDisplay =
        vsep
          [ hsep
              [ brackets $ string $ pack $ show sev,
                string "Your clause"
              ],
            indent 2 $ string $ pack $ show clause,
            string "makes use of the cut (!) operator." <> linebreak,
            hsep
              [ string "We have not introduced this operator yet.",
                string "Find a solution without it."
              ]
          ]
    }
