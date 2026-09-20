module CodeAnalysis.Rules.NoUnusedVariablesSpec where

import CodeAnalysis.Helper (shouldDetectProblemOfType, shouldNotDetectProblemOfType)
import Prolog.Programming.CodeAnalysis.Types
  ( ProblemType (NoUnusedVariables),
  )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = describe "NoUnusedVariables" $ do
  it "detects problem on example 1" $
    "p(X,Y) :- q(X)." `shouldDetectProblemOfType` NoUnusedVariables
  it "detects problem on example 2" $
    "p(X) :- X = [Z|Zs], q(Z)." `shouldDetectProblemOfType` NoUnusedVariables
  it "detects problem on example 3" $
    "p(X) :- X = [Z|Zs], q(Zs)." `shouldDetectProblemOfType` NoUnusedVariables

  it "doesn't detect problem on example 4" $
    "p(X) :- q(X)." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 5" $
    "p(X,X)." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 6" $
    "p(X) :- X = [Z|Zs], q(Z,Zs)." `shouldNotDetectProblemOfType` NoUnusedVariables

  it "detect problem on example 7" $
    "p :- a(X)." `shouldDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 8" $
    "p :- a(X), b(X)." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "detect problem on example 9" $
    "p(X) :- a(X), b(Y)." `shouldDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 10" $
    "p(X,Y) :- Z is X + Y, q(Z)." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 11" $
    "p(X,Y) :- X =:= Y." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 12" $
    "p(X,Y) :- X =\\= Y." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 13" $
    "p(X,Y) :- not(X =:= Y)." `shouldNotDetectProblemOfType` NoUnusedVariables
  it "doesn't detect problem on example 14" $
    "p(X,Y) :- X \\= Y." `shouldNotDetectProblemOfType` NoUnusedVariables
