{-# LANGUAGE RecordWildCards #-}

module Prolog.Programming.CodeAnalysis.Config
  ( configuredRules,
    defaultCodeAnalysisConfig,
  )
where

import Data.List (singleton)
import Prolog.Programming.CodeAnalysis.Rules.NoSingletonVariables (noSingletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Rules.Cuts (cutsRule)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig (..), Rule, Severity (..), WithSeverity (..))

configuredRules :: CodeAnalysisConfig -> [WithSeverity Rule]
configuredRules CodeAnalysisConfig {..} =
  ([WithSeverity Error cutsRule | not allowCutUsage])
    ++ maybe [] (singleton . flip WithSeverity noSingletonVariablesRule) noSingletonVariables

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig
    { noSingletonVariables = Just Warn,
      allowCutUsage = False
    }
