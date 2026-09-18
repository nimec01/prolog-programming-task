module Detection.Rules.RestrictCutUsageSpec where

import Language.Prolog
  ( Clause (..),
    Program,
    Term (..),
    VariableName (VariableName),
  )
import Prolog.Programming.Linting (checkForProblems)
import Prolog.Programming.Linting.Config (defaultDetectionConfig)
import Prolog.Programming.Linting.Types
  ( DetectionConfig (..),
    Problem (Problem, problemClause, problemHint, problemType),
    ProblemType (RestrictCutUsage),
    Severity (..),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

config :: DetectionConfig
config =
  DetectionConfig
    { hintProblems = [],
      warnProblems = [],
      errorProblems = [RestrictCutUsage]
    }

test :: Program -> [(Severity, Problem)]
test = checkForProblems config

spec :: Spec
spec = describe "RestrictCutUsage" $ do
  it "warns on example 1" $ do
    let clause =
          Clause
            { lhs = Struct "p" [Var (VariableName 0 "X")],
              rhs_ = [Struct "q" [Var (VariableName 0 "X")], Cut 0]
            }
     in test [clause]
          `shouldBe` [ ( Error,
                         Problem
                           { problemType = RestrictCutUsage,
                             problemClause = clause,
                             problemHint = Just "Don't use the cut (!) operator."
                           }
                       )
                     ]
  it "doesn't warn on example 2" $ do
    let term name = Struct name [Var (VariableName 0 "X"), Var (VariableName 0 "Y")]
        clause =
          Clause
            { lhs = term "p",
              rhs_ = [term "q"]
            }
     in test [clause]
          `shouldBe` []
