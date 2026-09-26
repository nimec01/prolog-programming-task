{-# LANGUAGE DeriveDataTypeable #-}

module Prolog.Programming.Types where

import Data.Data (Data)
import Data.Void (Void)
import Language.Prolog (Term)
import Prolog.Programming.CodeAnalysis.Types (CodeAnalysisConfig)

type TimeoutDuration = Int

data TreeStyle = QueryStyle | ResolutionStyle
  deriving Data

type IncludeTask = Include ()

type IncludeHidden = Include Void

data Include a = Yes | Filtered | No a
  deriving (Data, Eq)

type AllowListMatching = Bool

type ShowSWISHButton = Bool

data TaskConfig = TaskConfig {
  globalTimeout :: TimeoutDuration
  , treeStyle :: TreeStyle
  , includeTask :: IncludeTask
  , includeHidden :: IncludeHidden
  , allowListMatching :: AllowListMatching
  , displaySWISHButton :: ShowSWISHButton
  , codeAnalysisConfig :: CodeAnalysisConfig
  , specifications :: [Spec]
  }
  deriving Data

data TaskInstance = TaskInstance {
  taskConfig :: TaskConfig
  , sampleSolution :: String
  , visiblePredicates :: String
  , hiddenPredicates :: String
  }

data Spec = Spec {
  specVisibility :: Visibility
  , specVisualize :: Visualize
  , specExpection :: Expection
  , specTimeout :: Timeout
  , specRequirement :: Requirement
  }
  deriving (Data, Show)

data Visibility = Hidden String | Visible
  deriving (Data, Show)

data Visualize = ShowTree | DontShowTree
  deriving (Data, Show)

data Expection = PositiveResult | NegativeResult
  deriving (Data, Show)

data Timeout = GlobalTimeout | LocalTimeout Int
  deriving (Data, Show)

data Requirement
  = StatementToCheck [Term]
  | QueryWithAnswers [Term] [[Term]]
  | NewPredDecl Term String
  deriving (Data, Show)
