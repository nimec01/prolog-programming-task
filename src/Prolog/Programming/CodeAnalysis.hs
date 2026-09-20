{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Prolog.Programming.CodeAnalysis
  ( checkForProblems,
    displayProblems,
  )
where

import Data.List (groupBy, intersperse, uncons)
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause, Program, consultString)
import Prolog.Programming.CodeAnalysis.Config (configuredRules, defaultLintConfig)
import Prolog.Programming.CodeAnalysis.Helper (definesSamePredicate)
import Prolog.Programming.CodeAnalysis.Types (ConfiguredRule (..), LintConfig (..), Problem (..), Rule (..), Severity)
import Text.PrettyPrint.Leijen.Text (Doc, brackets, indent, linebreak, text, vsep, (<+>))

testCheck :: String -> IO [(Severity, Problem)]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems defaultLintConfig prog

checkForProblems :: LintConfig -> Program -> [(Severity, Problem)]
checkForProblems cfg clauses =
  filterFirstProblemPerClause $
    checkForProblems' cfg $
      groupBy definesSamePredicate clauses

checkForProblems' :: LintConfig -> [[Clause]] -> [(Severity, Problem)]
checkForProblems' cfg = concatMap (checkPredicateDefinitionsForProblem cfg)

checkPredicateDefinitionsForProblem :: LintConfig -> [Clause] -> [(Severity, Problem)]
checkPredicateDefinitionsForProblem cfg clauses =
  foldl
    (\acc configuredRule -> if null acc then map (severity configuredRule,) $ ruleDetect (rule configuredRule) clauses else acc)
    []
    $ configuredRules cfg

filterFirstProblemPerClause :: [(Severity, Problem)] -> [(Severity, Problem)]
filterFirstProblemPerClause pbs = mapMaybe (fmap fst . uncons) groupedByClause
  where
    groupedByClause = groupBy (\(_, a) (_, b) -> problemClause a == problemClause b) pbs

displayProblems :: [(Severity, Problem)] -> Doc
displayProblems pbs = vsep $ intersperse (text "-----") $ map displayProblem pbs

displayProblem :: (Severity, Problem) -> Doc
displayProblem (sev, Problem {..}) =
  vsep $
    [ brackets (text $ pack $ show sev) <+> text (pack $ "Found " ++ show problemType ++ " in clause:"),
      indent 2 $ text $ pack $ show problemClause
    ]
      ++ case problemHint of
        Nothing -> []
        Just msg -> [linebreak <> text (pack $ "Suggestion: " ++ msg)]
