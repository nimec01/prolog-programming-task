module CodeAnalysis.Rules.RestrictCutUsageSpec where

import CodeAnalysis.Helper (shouldDetectProblemsStrict, shouldNotDetectProblems)
import Data.List (isInfixOf)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
  )
import Test.Hspec (Expectation, Spec, describe, it)

caConfig :: CodeAnalysisConfig
caConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Nothing,
      restrictCutUsage = True
    }

detect :: String -> Bool
detect = isInfixOf "makes use of the cut (!) operator"

detectsProblem :: String -> Expectation
detectsProblem = shouldDetectProblemsStrict caConfig [detect]

doesNotDetectProblem :: String -> Expectation
doesNotDetectProblem = shouldNotDetectProblems caConfig [detect]

spec :: Spec
spec = describe "RestrictCutUsage" $ do
  it "detects problem on example 1" $
    detectsProblem "p(X) :- q(X), !."

  it "doesn't detect problem on example 2" $
    doesNotDetectProblem "p(X,Y) :- q(X,Y)."

  it "detects problem on example 4" $
    detectsProblem "p(X) :- a(X), (b(X), ! ; c(X))."
