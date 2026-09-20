{-# OPTIONS_GHC -Wno-orphans #-}

module Prolog.Programming.CodeAnalysis.Types
  ( ProblemType (..),
    Problem (..),
    Rule (..),
    CodeAnalysisConfig (..),
    ConfiguredRule (..),
    Severity (..),
  )
where

import Language.Prolog (Clause (..))
import Text.PrettyPrint.Leijen.Text (Doc)

data ProblemType
  = NoSingletonVariables
  | RestrictCutUsage
  deriving (Show, Eq)

data Problem = Problem
  { problemType :: ProblemType,
    problemClause :: Clause,
    problemDisplay :: Doc
  }
  deriving (Show)

newtype Rule = Rule
  { ruleDetect :: Clause -> [Problem]
  }

data CodeAnalysisConfig = CodeAnalysisConfig
  { noSingletonVariables :: Maybe Severity,
    restrictCutUsage :: Bool
  }

data Severity = Hint | Warn | Error
  deriving (Show, Eq)

data ConfiguredRule = ConfiguredRule
  { rule :: Rule,
    severity :: Severity
  }
