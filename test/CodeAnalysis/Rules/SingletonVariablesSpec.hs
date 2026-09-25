module CodeAnalysis.Rules.SingletonVariablesSpec where

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

caConfig :: CodeAnalysisConfig
caConfig =
  CodeAnalysisConfig
    { singletonVariables =
        SingletonVariablesConfig $ Detect Hint (),
      cutUsage =
        CutUsageConfig Ignore
    }

detect :: String -> Bool
detect = isInfixOf "includes the singleton variable"

detectsProblem :: String -> Expectation
detectsProblem = shouldDetectProblemsStrict caConfig [detect]

doesNotDetectProblem :: String -> Expectation
doesNotDetectProblem = shouldNotDetectProblems caConfig [detect]

spec :: Spec
spec = describe "NoSingletonVariables" $ do
  it "detects problem on example 1" $
    detectsProblem "p(X,Y) :- q(X)."
  it "detects problem on example 2" $
    detectsProblem "p(X) :- X = [Z|Zs], q(Z)."
  it "detects problem on example 3" $
    detectsProblem "p(X) :- X = [Z|Zs], q(Zs)."

  it "doesn't detect problems on example 4" $
    doesNotDetectProblem "p(X) :- q(X)."
  it "doesn't detect problems on example 5" $
    doesNotDetectProblem "p(X,X)."
  it "doesn't detect problems on example 6" $
    doesNotDetectProblem "p(X) :- X = [Z|Zs], q(Z,Zs)."

  it "detects problem on example 7" $
    detectsProblem "p :- a(X)."
  it "doesn't detect problems on example 8" $
    doesNotDetectProblem "p :- a(X), b(X)."
  it "detects problem on example 9" $
    detectsProblem "p(X) :- a(X), b(Y)."
  it "doesn't detect problems on example 10" $
    doesNotDetectProblem "p(X,Y) :- Z is X + Y, q(Z)."
  it "doesn't detect problems on example 11" $
    doesNotDetectProblem "p(X,Y) :- X =:= Y."
  it "doesn't detect problems on example 12" $
    doesNotDetectProblem "p(X,Y) :- X =\\= Y."
  it "doesn't detect problems on example 13" $
    doesNotDetectProblem "p(X,Y) :- not(X =:= Y)."
  it "doesn't detect problems on example 14" $
    doesNotDetectProblem "p(X,Y) :- X \\= Y."
  it "detects problem on example 15" $
    detectsProblem "p(X,f(Y)) :- g(X)."
  it "doesn't detect problems on example 16" $
    doesNotDetectProblem "p(X,Y) :- X > Y."
  it "doesn't detect problems on example 17" $
    doesNotDetectProblem "p(X,X,X)."
  it "doesn't detect problems on example 18" $
    doesNotDetectProblem "p(X,X,X,X)."

  it "doesn't detect problems on example 19" $
    doesNotDetectProblem "p(_)."
  it "detects problems on example 20" $
    detectsProblem "p(A,B)."
  it "detects problem on example 21" $
    detectsProblem "p(A,_)."
  it "doesn't detect problems on example 22" $
    doesNotDetectProblem "p(_A,_2)."

  it "doesn't detect problems on example 23" $
    doesNotDetectProblem "p(_,_)."
  it "detects problem on example 24" $
    detectsProblem "p(_A,_A)."
