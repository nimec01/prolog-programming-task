{-# OPTIONS_GHC -Wno-orphans #-}

module Prolog.Programming.CodeAnalysis.Types where

import Language.Prolog (Clause (..))

instance Eq Clause where
  Clause ls1 rs1 == Clause ls2 rs2 = ls1 == ls2 && rs1 == rs2
  _ == _ = False

data ProblemType
  = NoSingletonVariables
  | RestrictCutUsage
  deriving (Show, Eq)

data Problem = Problem
  { problemType :: ProblemType,
    problemClause :: Clause,
    problemHint :: Maybe String
  }
  deriving (Show, Eq)

data Rule = Rule
  { ruleDetect :: [Clause] -> [Problem],
    ruleProblemType :: ProblemType
  }

data CodeAnalysisConfig = CodeAnalysisConfig
  { noSingletonVariables :: Maybe Severity,
    restrictCutUsage :: Bool
  }

data Severity = Hint | Warn | Error
  deriving (Show, Eq)

data ConfiguredRule = ConfiguredRule
  { rule :: Rule,
    severity :: Maybe Severity
  }
