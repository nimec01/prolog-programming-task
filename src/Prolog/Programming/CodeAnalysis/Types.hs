module Prolog.Programming.CodeAnalysis.Types
  ( Problem (..),
    Rule,
    CodeAnalysisConfig (..),
    Severity (..),
    WithSeverity (..),
  )
where

import Language.Prolog (Clause (..))
import Text.PrettyPrint.Leijen.Text (Doc)

-- | Violation found in code
data Problem = Problem
  { -- | Clause the violation appears in
    problemClause :: Clause,
    -- | User-facing explanation of the violation
    problemDisplay :: Doc
  }
  deriving (Show)

-- | Definition for a code analysis checker that looks for violations in a given clause
type Rule = Clause -> [Problem]

-- | Configuration for code analysis checks
data CodeAnalysisConfig = CodeAnalysisConfig
  { -- | The severity to use when reporting singleton variables.
    -- `Nothing` results in no singleton variables being reported.
    singletonVariablesSeverity :: Maybe Severity,
    -- | Whether cuts are allowed to be used or not
    allowCutUsage :: Bool,
    -- | Optional message to show additionally next default explanation
    additionalCutUsageMessage :: Maybe String
  }

-- | Classification for seriousness of violation
--
-- The differentiation between `Hint` and `Warn` is of personal taste.
data Severity
  = -- | Minor violation of standard code conventions
    --
    -- Example violation: redundant braces
    Hint
  | -- | Moderate violation of standard code conventions
    --
    -- Example violation: working at the end of a list even though it is not necessary
    Warn
  | -- | Severe violation of standard code conventions or task restrictions
    --
    -- Example violation: cut operator was used even though the use of it was forbidden
    Error
  deriving (Show, Eq)

-- | Container for values with attached severity
data WithSeverity a = WithSeverity
  { severity :: Severity,
    value :: a
  }
