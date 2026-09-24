module CodeAnalysis.Rules.CutsSpec where

import CodeAnalysis.Helper (shouldDetectProblemsStrict, shouldNotDetectProblems)
import Data.List (isInfixOf)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    CodeAnalysisRuleConfig (..),
    CutUsageConfig (..),
    Severity (..),
    SingletonVariablesConfig (..),
  )
import Test.Hspec (Expectation, Spec, describe, it)

caConfig :: Maybe String -> CodeAnalysisConfig
caConfig cMsg =
  CodeAnalysisConfig
    { singletonVariables =
        SingletonVariablesConfig Ignore,
      cutUsage =
        CutUsageConfig $ Detect Error cMsg
    }

detect :: String -> Bool
detect = isInfixOf "makes use of the cut (!) operator"

detectsProblem :: String -> Expectation
detectsProblem = shouldDetectProblemsStrict (caConfig Nothing) [detect]

doesNotDetectProblem :: String -> Expectation
doesNotDetectProblem = shouldNotDetectProblems (caConfig Nothing) [detect]

spec :: Spec
spec = describe "Cuts" $ do
  it "detects problem on example 1" $
    detectsProblem "p(X) :- q(X), !."

  it "doesn't detect problem on example 2" $
    doesNotDetectProblem "p(X,Y) :- q(X,Y)."

  it "detects problem on example 4" $
    detectsProblem "p(X) :- a(X), (b(X), ! ; c(X))."

  it "detect problem with additional message" $
    shouldDetectProblemsStrict
      (caConfig $ Just "We have not introduced this operator yet.")
      [isInfixOf "We have not introduced this operator yet."]
      "p(X) :- q(X), !."
