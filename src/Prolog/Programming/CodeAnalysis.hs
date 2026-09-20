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
import Prolog.Programming.CodeAnalysis.Config (configuredRules, defaultCodeAnalysisConfig)
import Prolog.Programming.CodeAnalysis.Helper (definesSamePredicate)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), ConfiguredRule (..), Problem (..), Rule (..), Severity (Error))
import Text.PrettyPrint.Leijen.Text (Doc, text, vsep)

testCheck :: String -> IO [Problem]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems defaultCodeAnalysisConfig prog

checkForProblems :: CodeAnalysisConfig -> Program -> [Problem]
checkForProblems cfg clauses =
  filterFirstProblemPerClause $
    checkForProblems' cfg $
      groupBy definesSamePredicate clauses

checkForProblems' :: CodeAnalysisConfig -> [[Clause]] -> [Problem]
checkForProblems' cfg = concatMap (checkPredicateDefinitionsForProblem cfg)

checkPredicateDefinitionsForProblem :: CodeAnalysisConfig -> [Clause] -> [Problem]
checkPredicateDefinitionsForProblem cfg clauses =
  foldl
    (\acc configuredRule -> if null acc then ruleDetect (rule configuredRule) (severity configuredRule) clauses else acc)
    []
    $ configuredRules cfg

filterFirstProblemPerClause :: [Problem] -> [Problem]
filterFirstProblemPerClause pbs = mapMaybe (fmap fst . uncons) groupedByClause
  where
    groupedByClause = groupBy (\a b -> problemClause a == problemClause b) pbs

displayProblems :: [Problem] -> Either Doc Doc
displayProblems pbs = cons $ vsep $ intersperse (text $ pack $ replicate 15 '-') $ map problemDisplay pbs
  where
    cons = if any ((== Error) . problemSeverity) pbs then Left else Right
