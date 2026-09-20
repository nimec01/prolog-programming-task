module CodeAnalysis.Rules.NoSingletonVariablesSpec where

import CodeAnalysis.Helper (shouldDetectProblemOfType, shouldNotDetectProblemOfType)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    ProblemType (NoSingletonVariables),
    Severity (Warn),
  )
import Test.Hspec (Expectation, Spec, describe, it)

caConfig :: CodeAnalysisConfig
caConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Just Warn,
      restrictCutUsage = False
    }

detectsProblem :: String -> Expectation
detectsProblem = shouldDetectProblemOfType caConfig NoSingletonVariables

doesNotDetectProblem :: String -> Expectation
doesNotDetectProblem = shouldNotDetectProblemOfType caConfig NoSingletonVariables

spec :: Spec
spec = describe "NoSingletonVariables" $ do
  it "detects problem on example 1" $
    detectsProblem "p(X,Y) :- q(X)."
  it "detects problem on example 2" $
    detectsProblem "p(X) :- X = [Z|Zs], q(Z)."
  it "detects problem on example 3" $
    detectsProblem "p(X) :- X = [Z|Zs], q(Zs)."

  it "doesn't detect problem on example 4" $
    doesNotDetectProblem "p(X) :- q(X)."
  it "doesn't detect problem on example 5" $
    doesNotDetectProblem "p(X,X)."
  it "doesn't detect problem on example 6" $
    doesNotDetectProblem "p(X) :- X = [Z|Zs], q(Z,Zs)."

  it "detect problem on example 7" $
    detectsProblem "p :- a(X)."
  it "doesn't detect problem on example 8" $
    doesNotDetectProblem "p :- a(X), b(X)."
  it "detects problem on example 9" $
    detectsProblem "p(X) :- a(X), b(Y)."
  it "doesn't detect problem on example 10" $
    doesNotDetectProblem "p(X,Y) :- Z is X + Y, q(Z)."
  it "doesn't detect problem on example 11" $
    doesNotDetectProblem "p(X,Y) :- X =:= Y."
  it "doesn't detect problem on example 12" $
    doesNotDetectProblem "p(X,Y) :- X =\\= Y."
  it "doesn't detect problem on example 13" $
    doesNotDetectProblem "p(X,Y) :- not(X =:= Y)."
  it "doesn't detect problem on example 14" $
    doesNotDetectProblem "p(X,Y) :- X \\= Y."
  it "detects problem on example 15" $
    detectsProblem "p(X,f(Y)) :- g(X)."
  it "doesn't detect problem on example 16" $
    doesNotDetectProblem "p(X,Y) :- X > Y."
  it "doesn't detect proble on example 17" $
    doesNotDetectProblem "p(X,X,X)."
  it "doesn't detect proble on example 18" $
    doesNotDetectProblem "p(X,X,X,X)."
