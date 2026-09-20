module CodeAnalysis.Rules.RestrictCutUsageSpec where

import CodeAnalysis.Helper (shouldDetectProblemOfType, shouldNotDetectProblemOfType)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    ProblemType (RestrictCutUsage),
  )
import Test.Hspec (Expectation, Spec, describe, it)

caConfig :: CodeAnalysisConfig
caConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Nothing,
      restrictCutUsage = True
    }

detectsProblem :: String -> Expectation
detectsProblem = shouldDetectProblemOfType caConfig RestrictCutUsage

doesNotDetectProblem :: String -> Expectation
doesNotDetectProblem = shouldNotDetectProblemOfType caConfig RestrictCutUsage

spec :: Spec
spec = describe "RestrictCutUsage" $ do
  it "detects problem on example 1" $
    detectsProblem "p(X) :- q(X), !."

  it "doesn't detect problem on example 2" $
    doesNotDetectProblem "p(X,Y) :- q(X,Y)."

  it "detects problem on example 4" $
    detectsProblem "p(X) :- a(X), (b(X), ! ; c(X))."
