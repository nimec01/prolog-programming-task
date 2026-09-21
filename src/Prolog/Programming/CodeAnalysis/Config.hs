{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( configuredRules,
    defaultCodeAnalysisConfig,
  )
where

import Data.List (singleton)
import Prolog.Programming.CodeAnalysis.Rules.Cuts (cutsRule)
import Prolog.Programming.CodeAnalysis.Rules.SingletonVariables (singletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), Rule, Severity (..), WithSeverity (..))

configuredRules :: CodeAnalysisConfig -> [WithSeverity Rule]
configuredRules CodeAnalysisConfig {..} =
  ([WithSeverity Error cutsRule | not allowCutUsage])
    ++ maybe [] (singleton . flip WithSeverity singletonVariablesRule) singletonVariablesSeverity

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { singletonVariablesSeverity = Just Warn,
      allowCutUsage = False
    }
