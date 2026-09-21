{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( configuredRules,
    defaultCodeAnalysisConfig,
  )
where

import Data.List (singleton)
import Prolog.Programming.CodeAnalysis.Rules.NoSingletonVariables (noSingletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), Rule, Severity (..), WithSeverity (..))

configuredRules :: CodeAnalysisConfig -> [WithSeverity Rule]
configuredRules CodeAnalysisConfig {..} =
  ([WithSeverity restrictCutUsageRule Error | restrictCutUsage])
    ++ maybe [] (singleton . WithSeverity noSingletonVariablesRule) noSingletonVariables

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Just Warn,
      restrictCutUsage = False
    }
