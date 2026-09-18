{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.Linting.Config
  ( availableRules,
    configuredRules,
    defaultDetectionConfig,
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (mapMaybe)
import Prolog.Programming.Linting.Rules.NoUnusedVariables (noUnusedVariablesRule)
import Prolog.Programming.Linting.Rules.RestrictCutUsage (restrictCutUsageRule)
import Prolog.Programming.Linting.Types (ConfiguredRule (..), DetectionConfig (..), ProblemType (NoUnusedVariables, RestrictCutUsage), Rule (..), Severity (..))

availableRules :: [Rule]
availableRules =
  [ noUnusedVariablesRule,
    restrictCutUsageRule
  ]

configuredRules :: DetectionConfig -> [ConfiguredRule]
configuredRules cfg = mapMaybe configure availableRules
  where
    configure rule = ConfiguredRule rule <$> toSeverity cfg (ruleProblemType rule)

toSeverity :: DetectionConfig -> ProblemType -> Maybe Severity
toSeverity DetectionConfig {..} ptype =
  (Error <$ find (== ptype) errorProblems)
    <|> (Warn <$ find (== ptype) warnProblems)
    <|> (Hint <$ find (== ptype) hintProblems)

defaultDetectionConfig :: DetectionConfig
defaultDetectionConfig =
  DetectionConfig
    { hintProblems = [],
      warnProblems = [NoUnusedVariables],
      errorProblems = [RestrictCutUsage]
    }
