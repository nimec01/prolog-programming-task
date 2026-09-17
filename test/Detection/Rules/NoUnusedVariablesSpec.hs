module Detection.Rules.NoUnusedVariablesSpec where

import Language.Prolog
  ( Clause (..),
    Program,
    Term (Struct, Var),
    VariableName (VariableName),
  )
import Prolog.Programming.Detection (checkForProblems)
import Prolog.Programming.Detection.Config (defaultDetectionConfig)
import Prolog.Programming.Detection.Types
  ( DetectionConfig (DetectionConfig, errorProblems, hintProblems, warnProblems),
    Problem (Problem, problemClause, problemHint, problemType),
    ProblemType (NoUnusedVariables),
    Severity (..),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

config :: DetectionConfig
config =
  DetectionConfig
    { hintProblems = [],
      warnProblems = [NoUnusedVariables],
      errorProblems = []
    }

test :: Program -> [(Severity, Problem)]
test = checkForProblems config

spec :: Spec
spec = describe "NoUnusedVariables" $ do
  it "warns on example 1" $ do
    let clause =
          Clause
            { lhs = Struct "p" [Var (VariableName 0 "X"), Var (VariableName 0 "Y")],
              rhs_ = [Struct "q" [Var (VariableName 0 "X")]]
            }
     in test [clause]
          `shouldBe` [ ( Warn,
                         Problem
                           { problemType = NoUnusedVariables,
                             problemClause = clause,
                             problemHint = Just "Replace Y with wildcard (_) ."
                           }
                       )
                     ]
  it "warns on example 2" $ do
    let clause =
          Clause
            { lhs = Struct "p" [Var (VariableName 0 "X")],
              rhs_ =
                [ Struct "=" [Var (VariableName 0 "X"), Struct "." [Var (VariableName 0 "Z"), Var (VariableName 0 "Zs")]],
                  Struct "q" [Var (VariableName 0 "Zs")]
                ]
            }
     in test [clause]
          `shouldBe` [ ( Warn,
                         Problem
                           { problemType = NoUnusedVariables,
                             problemClause = clause,
                             problemHint = Just "Replace Z with wildcard (_) ."
                           }
                       )
                     ]
  it "warns on example 3" $ do
    let clause =
          Clause
            { lhs = Struct "p" [Var (VariableName 0 "X")],
              rhs_ =
                [ Struct "=" [Var (VariableName 0 "X"), Struct "." [Var (VariableName 0 "Z"), Var (VariableName 0 "Zs")]],
                  Struct "q" [Var (VariableName 0 "Z")]
                ]
            }
     in test [clause]
          `shouldBe` [ ( Warn,
                         Problem
                           { problemType = NoUnusedVariables,
                             problemClause = clause,
                             problemHint = Just "Replace Zs with wildcard (_) ."
                           }
                       )
                     ]
  it "doesn't warn on example 4" $ do
    let term name = Struct name [Var (VariableName 0 "X"), Var (VariableName 0 "Y")]
        clause =
          Clause
            { lhs = term "p",
              rhs_ = [term "q"]
            }
     in test [clause]
          `shouldBe` []
