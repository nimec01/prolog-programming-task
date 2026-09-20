module Linting.Rules.RestrictCutUsageSpec where

import Linting.Helper (shouldDetectProblemOfType, shouldNotDetectProblemOfType)
import Prolog.Programming.Linting.Types
  ( ProblemType (RestrictCutUsage),
  )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = describe "RestrictCutUsage" $ do
  it "detects problem on example 1" $
    "p(X) :- q(X), !." `shouldDetectProblemOfType` RestrictCutUsage

  it "doesn't detect problem on example 2" $
    "p(X,Y) :- q(X,Y)." `shouldNotDetectProblemOfType` RestrictCutUsage

  it "detects problem on example 4" $
    "p(X) :- a(X), (b(X), ! ; c(X))." `shouldDetectProblemOfType` RestrictCutUsage
