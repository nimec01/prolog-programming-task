{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( configuredRules,
    defaultSingletonVariablesConfig,
    defaultCutUsageConfig,
    defaultCodeAnalysisConfig,
  )
where

import Prolog.Programming.CodeAnalysis.Rules.Cuts (cutsRule)
import Prolog.Programming.CodeAnalysis.Rules.SingletonVariables (singletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Types
  ( CodeAnalysisConfig (..),
    CutUsageConfig (..),
    Rule,
    Severity (..),
    SingletonVariablesConfig (..),
    WithSeverity (..),
  )

configuredRules :: CodeAnalysisConfig -> [WithSeverity Rule]
configuredRules
  CodeAnalysisConfig
    { cutUsage = CutUsageConfig {..},
      singletonVariables = SingletonVariablesConfig {..}
    } =
    [WithSeverity cutUsageSeverity (cutsRule cutUsageMessage) | not allowCutUsage]
      ++ [WithSeverity singletonVariablesSeverity singletonVariablesRule | not allowSingletonVariables]

defaultSingletonVariablesConfig :: SingletonVariablesConfig
defaultSingletonVariablesConfig =
  SingletonVariablesConfig
    { allowSingletonVariables = True,
      singletonVariablesSeverity = Hint
    }

defaultCutUsageConfig :: CutUsageConfig
defaultCutUsageConfig =
  CutUsageConfig
    { allowCutUsage = True,
      cutUsageSeverity = Error,
      cutUsageMessage = Nothing
    }

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { singletonVariables = defaultSingletonVariablesConfig,
      cutUsage = defaultCutUsageConfig
    }
