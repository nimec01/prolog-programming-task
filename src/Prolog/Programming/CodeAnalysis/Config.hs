module Prolog.Programming.CodeAnalysis.Config (
  configuredRules,
  defaultCodeAnalysisConfig,
  escalateConfiguredRules,
)
where

import Data.Maybe (catMaybes)
import Generics.SYB (everywhere, mkT)
import Prolog.Programming.CodeAnalysis.Rules.Cuts (cutsRule)
import Prolog.Programming.CodeAnalysis.Rules.SingletonVariables (singletonVariablesRule)
import Prolog.Programming.CodeAnalysis.Types (
  CodeAnalysisConfig (..),
  CodeAnalysisRuleConfig (..),
  CutUsageConfig (..),
  Rule,
  Severity (..),
  SingletonVariablesConfig (..),
  WithSeverity (..),
 )

configuredRules :: CodeAnalysisConfig -> [WithSeverity Rule]
configuredRules
  CodeAnalysisConfig {
    singletonVariables = SingletonVariablesConfig singletonVarsCfg
    , cutUsage = CutUsageConfig cutsCfg
    } =
    catMaybes
      [ toConfigured singletonVarsCfg (const singletonVariablesRule)
      , toConfigured cutsCfg cutsRule
      ]
    where
      toConfigured :: CodeAnalysisRuleConfig a -> (a -> Rule) -> Maybe (WithSeverity Rule)
      toConfigured Ignore _ = Nothing
      toConfigured (Detect severity' extra) build = Just (WithSeverity severity' (build extra))

defaultCodeAnalysisConfig :: CodeAnalysisConfig
defaultCodeAnalysisConfig =
  CodeAnalysisConfig {
    singletonVariables = SingletonVariablesConfig Ignore
    , cutUsage = CutUsageConfig Ignore
    }

escalateConfiguredRules :: CodeAnalysisConfig -> CodeAnalysisConfig
escalateConfiguredRules = everywhere $ mkT toError
  where
    toError :: Severity -> Severity
    toError _ = Error
