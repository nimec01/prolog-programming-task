{-# LANGUAGE OverloadedStrings #-}

module Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule) where

import Data.Text.Lazy (pack)
import Language.Prolog (Clause (..))
import Prolog.Programming.CodeAnalysis.Helper (containsCut)
import Prolog.Programming.CodeAnalysis.Types (Problem (..), ProblemType (RestrictCutUsage), Rule)
import Text.PrettyPrint.Leijen.Text (hsep, indent, linebreak, string, vsep)

restrictCutUsageRule :: Rule
restrictCutUsageRule clause
  | cutExistsInClause clause = [toProblem clause]
  | otherwise = []

cutExistsInClause :: Clause -> Bool
cutExistsInClause (Clause _ rhs) = any containsCut rhs
cutExistsInClause _ = False

toProblem :: Clause -> Problem
toProblem clause =
  Problem
    { problemType = RestrictCutUsage,
      problemClause = clause,
      problemDisplay =
        vsep
          [ string "Your clause",
            indent 2 $ string $ pack $ show clause,
            string "makes use of the cut (!) operator." <> linebreak,
            hsep
              [ string "We have not introduced this operator yet.",
                string "Find a solution without it."
              ]
          ]
    }
