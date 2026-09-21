module Prolog.Programming.CodeAnalysis.Types
  ( ProblemType (..),
    Problem (..),
    Rule (..),
    CodeAnalysisConfig (..),
    Severity (..),
    WithSeverity (..),
  )
where

import Language.Prolog (Clause (..))
import Text.PrettyPrint.Leijen.Text (Doc)

-- | Type of violation
data ProblemType
  = NoSingletonVariables
  | RestrictCutUsage
  deriving (Show, Eq)

-- | Violation found in code
data Problem = Problem
  { -- | Type of violation
    problemType :: ProblemType,
    -- | Clause the violation appears in
    -- | User-facing explanation of the violation
    problemClause :: Clause,
    problemDisplay :: Doc
  }
  deriving (Show)

-- | Definition for a code analysis check
newtype Rule = Rule
  { -- | Function that checks for violations in a given clause
    ruleDetect :: Clause -> [Problem]
  }

-- | Configuration for code analysis checks
data CodeAnalysisConfig = CodeAnalysisConfig
  { -- | Whether singleton variables are allowed or not and what severity to use while reporting.
    noSingletonVariables :: Maybe Severity,
    -- | Whether cuts are allowed to be used or not
    restrictCutUsage :: Bool
  }

-- | Classification for seriousness of violation
--
-- The differentiation between `Hint` and `Warn` is of personal taste.
data Severity
  = -- | Optional improvements that might increase style or readability
    Hint
  | -- | Recommended improvements that should keep program semantics
    Warn
  | -- | Serious issue whose fix might change program semantics
    Error
  deriving (Show, Eq)

-- | Container for values with attached severity
data WithSeverity a = WithSeverity
  { value :: a,
    severity :: Severity
  }
