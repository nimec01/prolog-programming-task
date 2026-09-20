module Linting.Helper where

import Language.Prolog (consultString)
import Prolog.Programming.Linting (checkForProblems)
import Prolog.Programming.Linting.Types (LintConfig (..), Problem (problemType), ProblemType, Severity (..))
import Test.HUnit (assertFailure)
import Test.Hspec (Expectation)

prepareConfig :: (Severity, ProblemType) -> LintConfig
prepareConfig (sev, pt) = case sev of
  Hint -> emptyConfig {hintProblems = [pt]}
  Warn -> emptyConfig {warnProblems = [pt]}
  Error -> emptyConfig {errorProblems = [pt]}
  where
    emptyConfig =
      LintConfig
        { hintProblems = [],
          warnProblems = [],
          errorProblems = []
        }

shouldDetectProblemOfType :: String -> ProblemType -> Expectation
shouldDetectProblemOfType code pt = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems (prepareConfig (Hint, pt)) prog of
    [] -> assertFailure "No problem with provided type found"
    pbs | any (\(_, pb) -> pt /= problemType pb) pbs -> assertFailure "Found problem does not match provided one."
    _ -> pure ()

shouldNotDetectProblemOfType :: String -> ProblemType -> Expectation
shouldNotDetectProblemOfType code pt = case consultString code of
  Left err -> assertFailure $ "Failed to parse prolog program:\n" ++ show err
  Right prog -> case checkForProblems (prepareConfig (Hint, pt)) prog of
    [] -> pure ()
    pbs | any (\(_, pb) -> pt == problemType pb) pbs -> assertFailure "Found problem that should not exist."
    _ -> pure ()
