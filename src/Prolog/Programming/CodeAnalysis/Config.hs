{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( availableRules,
    configuredRules,
    defaultCodeAnalysisConfig,
  )
where

import Data.List (singleton)
import Prolog.Programming.CodeAnalysis.Rules.NoSingletonVariables (noSingletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), ConfiguredRule (..), Rule (..), Severity (..))

availableRules :: [Rule]
availableRules =
  [ noSingletonVariablesRule,
    restrictCutUsageRule
  ]

configuredRules :: CodeAnalysisConfig -> [ConfiguredRule]
configuredRules CodeAnalysisConfig {..} =
  ([ConfiguredRule restrictCutUsageRule Nothing | restrictCutUsage]) ++ maybe [] (singleton . ConfiguredRule noSingletonVariablesRule . Just) noSingletonVariables

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Just Warn,
      restrictCutUsage = False
    }
