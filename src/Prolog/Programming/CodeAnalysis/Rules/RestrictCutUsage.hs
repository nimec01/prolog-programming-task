{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule) where

import Data.Data (Data)
import Data.Generics (everything, mkQ)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause (..), Term (..))
import Prolog.Programming.CodeAnalysis.Types (Problem (..), ProblemType (RestrictCutUsage), Rule)
import Text.PrettyPrint.Leijen.Text (hsep, indent, linebreak, string, vsep)

restrictCutUsageRule :: Rule
restrictCutUsageRule clause
  | cutExistsInClause clause = [toProblem clause]
  | otherwise = []

cutExistsInClause :: Clause -> Bool
cutExistsInClause (Clause _ rhs) = any containsCut rhs
cutExistsInClause _ = False

containsCut :: (Data a) => a -> Bool
containsCut = everything (||) $ mkQ False $ \case
  Cut _ -> True
  _ -> False

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
