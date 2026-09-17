{-# LANGUAGE TupleSections #-}

module Prolog.Programming.Detection.Rules.NoUnusedVariables (noUnusedVariables) where

import Data.List (uncons, (\\))
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
clauseHasUnusedVariable (Clause (Struct _ args) rs) = case uncons args of
  Nothing -> Nothing
  Just (x, xs) -> case namedVariablesInTerm x of
    [] -> Nothing
    (v : _) -> fst <$> uncons (unusedVariables [v] (xs ++ rs))
clauseHasUnusedVariable _ = Nothing

unusedVariables :: [String] -> [Term] -> [String]
unusedVariables vs [] = vs
unusedVariables vs (t : ts) = unusedVariables ((vs \\ varsInT) ++ (varsInT \\ vs)) ts
  where
    varsInT = namedVariablesInTerm t

-- termUsesVariables :: [String] -> Term -> Bool
-- termUsesVariables vs term = not $ null $ vs `intersect` namedVariablesInTerm term

toProblem :: (Clause, String) -> Problem
toProblem (clause, unused) =
  Problem
    { problemType = NoUnusedVariables,
      problemClause = clause,
      problemHint = Just $ "Replace " ++ unused ++ " with wildcard (_) ."
    }
