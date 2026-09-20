{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Prolog.Programming.CodeAnalysis
  ( checkForProblems,
    displayProblems,
  )
where

import Data.List (groupBy)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause, Program, consultString)
import Prolog.Programming.CodeAnalysis.Config (configuredRules, defaultCodeAnalysisConfig)
import Prolog.Programming.CodeAnalysis.Helper (definesSamePredicate)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), ConfiguredRule (..), Problem (..), Rule (..), Severity (Error))
import Text.PrettyPrint.Leijen.Text (Doc, brackets, string, vsep, (<$$>))

testCheck :: String -> IO [(Problem, Severity)]
testCheck code = case consultString code of
  Left err -> do
    print err
    pure []
  Right prog -> pure $ checkForProblems defaultCodeAnalysisConfig prog

checkForProblems :: CodeAnalysisConfig -> Program -> [(Problem, Severity)]
checkForProblems cfg clauses =
  checkForProblems' cfg $
    groupBy definesSamePredicate clauses

checkForProblems' :: CodeAnalysisConfig -> [[Clause]] -> [(Problem, Severity)]
checkForProblems' cfg = concatMap (checkPredicateDefinitionsForProblem cfg)

checkPredicateDefinitionsForProblem :: CodeAnalysisConfig -> [Clause] -> [(Problem, Severity)]
checkPredicateDefinitionsForProblem cfg clauses =
  foldl
    (\acc configuredRule -> ((,severity configuredRule) <$> ruleDetect (rule configuredRule) clauses) ++ acc)
    []
    $ configuredRules cfg

displayProblems :: [(Problem, Severity)] -> Either Doc Doc
displayProblems pbs =
  cons $
    vsep $
      map (\(p, sev) -> padEnd 30 "-" (brackets $ string $ pack $ show sev) <$$> problemDisplay p) pbs
  where
    cons = if any ((== Error) . snd) pbs then Left else Right

padEnd :: Int -> String -> Doc -> Doc
padEnd maxWidth filler x = x <> mconcat (replicate (max 0 (maxWidth - width x)) $ string $ pack filler)

width :: Doc -> Int
width doc = length $ show doc
