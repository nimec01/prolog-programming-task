{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( configuredRules,
    defaultCodeAnalysisConfig,
  )
where

import Data.List (singleton)
import Prolog.Programming.CodeAnalysis.Rules.NoSingletonVariables (noSingletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Rules.RestrictCutUsage (restrictCutUsageRule)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), ConfiguredRule (..), Severity (..))

configuredRules :: CodeAnalysisConfig -> [ConfiguredRule]
configuredRules CodeAnalysisConfig {..} =
  ([ConfiguredRule restrictCutUsageRule Error | restrictCutUsage]) ++ maybe [] (singleton . ConfiguredRule noSingletonVariablesRule) noSingletonVariables

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Just Warn,
      restrictCutUsage = False
    }
