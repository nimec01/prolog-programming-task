{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import Prolog.Programming.Detection.Config (configuredRules, defaultDetectionConfig)
import Prolog.Programming.Detection.Helper (definesSamePredicate)
import Prolog.Programming.Detection.Types (ConfiguredRule (..), DetectionConfig (..), Problem (..), Rule (..), Severity)
import Text.PrettyPrint.Leijen.Text (Doc, indent, text, vsep)

testCheck :: String -> IO [(Severity, Problem)]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems defaultDetectionConfig prog

checkForProblems :: DetectionConfig -> Program -> [(Severity, Problem)]
checkForProblems cfg clauses =
  filterFirstProblemPerClause $
    checkForProblems' cfg $
      groupBy definesSamePredicate clauses

checkForProblems' :: DetectionConfig -> [[Clause]] -> [(Severity, Problem)]
checkForProblems' cfg = concatMap (checkPredicateDefinitionsForProblem cfg)

checkPredicateDefinitionsForProblem :: DetectionConfig -> [Clause] -> [(Severity, Problem)]
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
    [ text "For Clause:",
      indent 2 $ text $ pack $ show problemClause,
      text "Type: " <> text (pack $ show problemType),
      text "Severity: " <> text (pack $ show sev)
    ]
      ++ case problemHint of
        Nothing -> []
        Just msg -> [text $ pack $ "Hint: " ++ msg]
