module CodeAnalysis.Helper where

import Language.Prolog (consultString)
import Prolog.Programming.CodeAnalysis (checkForProblems)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    Problem (problemDisplay),
    WithSeverity (..),
  )
import Test.HUnit (assertFailure)
import Test.Hspec (Expectation)

shouldDetectProblemsStrict :: CodeAnalysisConfig -> [String -> Bool] -> String -> Expectation
shouldDetectProblemsStrict cfg pts code = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems cfg prog of
    [] -> assertFailure "No problems found"
    pbs
      | length pbs /= length pts -> assertFailure "More or less problems found than provided"
      | all (\t -> any (t . show . problemDisplay . value) pbs) pts -> pure ()
      | otherwise -> assertFailure "Found problems that does not match"

shouldNotDetectProblems :: CodeAnalysisConfig -> [String -> Bool] -> String -> Expectation
shouldNotDetectProblems cfg pts code = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems cfg prog of
    [] -> pure ()
    pbs
      | any (\t -> any (t . show . problemDisplay . value) pbs) pts ->
          assertFailure "Detected problem that should not occur"
      | otherwise -> pure ()
