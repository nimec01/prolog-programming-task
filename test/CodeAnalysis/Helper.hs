module CodeAnalysis.Helper where

import Language.Prolog (consultString)
import Prolog.Programming.CodeAnalysis (checkForProblems)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), Problem (problemType), ProblemType)
import Test.HUnit (assertFailure)
import Test.Hspec (Expectation)

shouldDetectProblemOfType :: CodeAnalysisConfig -> ProblemType -> String -> Expectation
shouldDetectProblemOfType cfg pt code = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems cfg prog of
    [] -> assertFailure "No problem with provided type found"
    pbs | any (\pb -> pt /= problemType pb) pbs -> assertFailure "Found problem does not match provided one."
    _ -> pure ()

shouldNotDetectProblemOfType :: CodeAnalysisConfig -> ProblemType -> String -> Expectation
shouldNotDetectProblemOfType cfg pt code = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems cfg prog of
    [] -> pure ()
    pbs | any (\pb -> pt == problemType pb) pbs -> assertFailure "Found problem that should not exist."
    _ -> pure ()

shouldDetectProblemsOfTypeStrict :: CodeAnalysisConfig -> [ProblemType] -> String -> Expectation
shouldDetectProblemsOfTypeStrict cfg pts code = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems cfg prog of
    [] -> assertFailure "No problems found"
    pbs
      | length pbs /= length pts -> assertFailure "More or less problems found than provided"
      | all (\t -> any ((== t) . problemType) pbs) pts -> pure ()
      | otherwise -> assertFailure "Problem types do not match"
