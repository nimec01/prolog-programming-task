{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( availableRules,
    configuredRules,
    defaultLintConfig,
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (mapMaybe)
import Prolog.Programming.CodeAnalysis.Rules.NoUnusedVariables (noUnusedVariablesRule)
import Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule)
import Prolog.Programming.CodeAnalysis.Types (ConfiguredRule (..), LintConfig (..), ProblemType (NoUnusedVariables, RestrictCutUsage), Rule (..), Severity (..))

availableRules :: [Rule]
availableRules =
  [ noUnusedVariablesRule,
    restrictCutUsageRule
  ]

configuredRules :: LintConfig -> [ConfiguredRule]
configuredRules cfg = mapMaybe configure availableRules
  where
    configure rule = ConfiguredRule rule <$> toSeverity cfg (ruleProblemType rule)

toSeverity :: LintConfig -> ProblemType -> Maybe Severity
toSeverity LintConfig {..} ptype =
  (Error <$ find (== ptype) errorProblems)
    <|> (Warn <$ find (== ptype) warnProblems)
    <|> (Hint <$ find (== ptype) hintProblems)

defaultLintConfig :: LintConfig
defaultLintConfig =
  LintConfig
    { hintProblems = [],
      warnProblems = [NoUnusedVariables],
      errorProblems = [RestrictCutUsage]
    }
