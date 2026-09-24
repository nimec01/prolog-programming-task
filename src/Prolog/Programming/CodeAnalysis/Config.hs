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
    SingletonVariablesConfig (..),
    WithSeverity (..),
  )

configuredRules :: CodeAnalysisConfig -> [WithSeverity Rule]
configuredRules
  CodeAnalysisConfig
    { cutUsage = CutUsageConfig {..},
      singletonVariables = SingletonVariablesConfig {..}
    } =
    maybe
      []
      (\severity -> [WithSeverity severity (cutsRule cutUsageMessage)])
      cutUsageSeverity
      ++ maybe
        []
        (\severity -> [WithSeverity severity singletonVariablesRule])
        singletonVariablesSeverity

defaultSingletonVariablesConfig :: SingletonVariablesConfig
defaultSingletonVariablesConfig =
  SingletonVariablesConfig
    { singletonVariablesSeverity = Nothing
    }

defaultCutUsageConfig :: CutUsageConfig
defaultCutUsageConfig =
  CutUsageConfig
    { cutUsageSeverity = Nothing,
      cutUsageMessage = Nothing
    }

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { singletonVariables = defaultSingletonVariablesConfig,
      cutUsage = defaultCutUsageConfig
    }
