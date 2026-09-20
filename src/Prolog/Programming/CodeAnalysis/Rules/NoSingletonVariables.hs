{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Prolog.Programming.CodeAnalysis.Rules.NoSingletonVariables (noSingletonVariablesRule) where

import Data.List (uncons, (\\))
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause (..), Term (..))
import Prolog.Programming.CodeAnalysis.Helper (namedVariablesInTerm)
import Prolog.Programming.CodeAnalysis.Types (Problem (..), ProblemType (NoSingletonVariables), Rule (..), Severity)
import Text.PrettyPrint.Leijen.Text (brackets, hsep, indent, linebreak, string, vsep)

noSingletonVariablesRule :: Rule
noSingletonVariablesRule =
  Rule
    { ruleDetect = detect
    }

detect :: Severity -> [Clause] -> [Problem]
detect sev predicateDefs = map (toProblem sev) clausesWithSingletonVariable
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

toProblem :: Severity -> (Clause, String) -> Problem
toProblem sev (clause, var) =
  Problem
    { problemType = NoSingletonVariables,
      problemClause = clause,
      problemSeverity = sev,
      problemDisplay =
        vsep
          [ hsep
              [ brackets $ string $ pack $ show sev,
                string "Your clause"
              ],
            indent 2 $ string $ pack $ show clause,
            string (pack $ "includes the singleton variable " ++ var ++ ".") <> linebreak,
            string "You can safely replace it with a wildcard (_)."
          ]
    }
