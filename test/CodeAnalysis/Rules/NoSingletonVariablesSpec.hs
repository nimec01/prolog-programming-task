module CodeAnalysis.Rules.NoSingletonVariablesSpec where

import CodeAnalysis.Helper (shouldDetectProblemOfType, shouldNotDetectProblemOfType)
import Prolog.Programming.CodeAnalysis.Types
  ( ProblemType (NoSingletonVariables),
  )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = describe "NoSingletonVariables" $ do
  it "detects problem on example 1" $
    "p(X,Y) :- q(X)." `shouldDetectProblemOfType` NoSingletonVariables
  it "detects problem on example 2" $
    "p(X) :- X = [Z|Zs], q(Z)." `shouldDetectProblemOfType` NoSingletonVariables
  it "detects problem on example 3" $
    "p(X) :- X = [Z|Zs], q(Zs)." `shouldDetectProblemOfType` NoSingletonVariables

  it "doesn't detect problem on example 4" $
    "p(X) :- q(X)." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 5" $
    "p(X,X)." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 6" $
    "p(X) :- X = [Z|Zs], q(Z,Zs)." `shouldNotDetectProblemOfType` NoSingletonVariables

  it "detect problem on example 7" $
    "p :- a(X)." `shouldDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 8" $
    "p :- a(X), b(X)." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "detect problem on example 9" $
    "p(X) :- a(X), b(Y)." `shouldDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 10" $
    "p(X,Y) :- Z is X + Y, q(Z)." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 11" $
    "p(X,Y) :- X =:= Y." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 12" $
    "p(X,Y) :- X =\\= Y." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 13" $
    "p(X,Y) :- not(X =:= Y)." `shouldNotDetectProblemOfType` NoSingletonVariables
  it "doesn't detect problem on example 14" $
    "p(X,Y) :- X \\= Y." `shouldNotDetectProblemOfType` NoSingletonVariables
