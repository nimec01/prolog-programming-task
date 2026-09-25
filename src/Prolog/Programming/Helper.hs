{-# LANGUAGE NamedFieldPuns #-}

module Prolog.Programming.Helper where

import Language.Prolog (Atom, Term (..))
import Prolog.Programming.CodeAnalysis.Config (escalateConfiguredRules)
import Prolog.Programming.Types (TaskConfig (TaskConfig, codeAnalysisConfig))

type Arity = Int
termHead :: Term -> (Atom, Arity)
termHead (Struct hd args) = (hd, length args)
termHead _ = error "can't extract clause head"

escalateCodeAnalysis :: TaskConfig -> TaskConfig
escalateCodeAnalysis cfg@TaskConfig {codeAnalysisConfig} =
  cfg {codeAnalysisConfig = escalateConfiguredRules codeAnalysisConfig}
