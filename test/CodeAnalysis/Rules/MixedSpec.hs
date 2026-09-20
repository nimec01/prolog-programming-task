module CodeAnalysis.Rules.MixedSpec where

import CodeAnalysis.Helper (shouldDetectProblemsOfTypeStrict)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    ProblemType (..),
    Severity (Warn),
  )
import Test.Hspec (Expectation, Spec, describe, it)

caConfig :: CodeAnalysisConfig
caConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Just Warn,
      restrictCutUsage = True
    }

detectsProblems :: String -> Expectation
detectsProblems = shouldDetectProblemsOfTypeStrict caConfig [NoSingletonVariables, RestrictCutUsage]

spec :: Spec
spec = describe "Mixed rule tests" $ do
  it "singleton + cut" $
    detectsProblems "a(X) :- X = [Z|Zs], ! , b(Zs)."
