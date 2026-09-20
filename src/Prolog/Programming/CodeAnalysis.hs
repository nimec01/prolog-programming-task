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
import Data.Maybe (isNothing, mapMaybe)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause, Program, consultString)
import Prolog.Programming.CodeAnalysis.Config (configuredRules, defaultCodeAnalysisConfig)
import Prolog.Programming.CodeAnalysis.Helper (definesSamePredicate)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), ConfiguredRule (..), Problem (..), Rule (..), Severity (Error))
import Text.PrettyPrint.Leijen.Text (Doc, text, vsep)

testCheck :: String -> IO [(Maybe Severity, Problem)]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems defaultCodeAnalysisConfig prog

checkForProblems :: CodeAnalysisConfig -> Program -> [(Maybe Severity, Problem)]
checkForProblems cfg clauses =
  filterFirstProblemPerClause $
    checkForProblems' cfg $
      groupBy definesSamePredicate clauses

checkForProblems' :: CodeAnalysisConfig -> [[Clause]] -> [(Maybe Severity, Problem)]
checkForProblems' cfg = concatMap (checkPredicateDefinitionsForProblem cfg)

checkPredicateDefinitionsForProblem :: CodeAnalysisConfig -> [Clause] -> [(Maybe Severity, Problem)]
checkPredicateDefinitionsForProblem cfg clauses =
  foldl
    (\acc configuredRule -> if null acc then map (severity configuredRule,) $ ruleDetect (rule configuredRule) clauses else acc)
    []
    $ configuredRules cfg

filterFirstProblemPerClause :: [(Maybe Severity, Problem)] -> [(Maybe Severity, Problem)]
filterFirstProblemPerClause pbs = mapMaybe (fmap fst . uncons) groupedByClause
  where
    groupedByClause = groupBy (\(_, a) (_, b) -> problemClause a == problemClause b) pbs

displayProblems :: [(Maybe Severity, Problem)] -> Either Doc Doc
displayProblems pbs = cons $ vsep $ intersperse (text $ pack $ replicate 15 '-') $ map displayProblem pbs
  where
    cons = if any (\(ms, _) -> ms == Just Error || isNothing ms) pbs then Left else Right

displayProblem :: (Maybe Severity, Problem) -> Doc
displayProblem (mSev, Problem {..}) =
  problemDisplay mSev
