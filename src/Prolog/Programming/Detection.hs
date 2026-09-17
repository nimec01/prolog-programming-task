{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.Detection
  ( testCheck,
    checkForProblems,
    displayProblems,
  )
where

import Data.List (groupBy, intersperse, uncons)
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause, Program, consultString)
import Prolog.Programming.Detection.Helper (definesSamePredicate)
import Prolog.Programming.Detection.Rules.NoUnusedVariables (noUnusedVariables)
import Prolog.Programming.Detection.Rules.RestrictCutUsage (restrictCutUsageRule)
import Prolog.Programming.Detection.Types (DetectionConfig (DetectionConfig), Problem (..), Rule (..))
import Text.PrettyPrint.Leijen.Text (Doc, indent, text, vsep)

rules :: [Rule]
rules =
  [ noUnusedVariables,
    restrictCutUsageRule
  ]

testCheck :: String -> IO [Problem]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems (DetectionConfig {}) prog

checkForProblems :: DetectionConfig -> Program -> [Problem]
checkForProblems _ clauses =
  filterFirstProblemPerClause $
    checkForProblems' $
      groupBy definesSamePredicate clauses

checkForProblems' :: [[Clause]] -> [Problem]
checkForProblems' = concatMap checkPredicateDefinitionsForProblem

checkPredicateDefinitionsForProblem :: [Clause] -> [Problem]
checkPredicateDefinitionsForProblem clauses =
  foldl
    (\acc rule -> if null acc then detectProblems rule clauses else acc)
    []
    rules

filterFirstProblemPerClause :: [Problem] -> [Problem]
filterFirstProblemPerClause pbs = mapMaybe (fmap fst . uncons) groupedByClause
  where
    groupedByClause = groupBy (\a b -> problemClause a == problemClause b) pbs

displayProblems :: [Problem] -> Doc
displayProblems pbs = vsep $ intersperse (text "-----") $ map displayProblem pbs

displayProblem :: Problem -> Doc
displayProblem Problem {..} =
  vsep $
    [ text "For Clause:",
      indent 2 $ text $ pack $ show problemClause,
      text "Type: " <> text (pack $ show problemType)
    ]
      ++ case hint of
        Nothing -> []
        Just msg -> [text $ pack $ "Hint: " ++ msg]
