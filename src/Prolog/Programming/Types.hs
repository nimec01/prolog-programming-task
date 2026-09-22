module Prolog.Programming.Types where

import Data.Void (Void)
import Language.Prolog (Term)

type TimeoutDuration = Int

data TreeStyle = QueryStyle | ResolutionStyle

type IncludeTask = Include ()

type IncludeHidden = Include Void

data Include a = Yes | Filtered | No a

type AllowListMatching = Bool

type ShowSWISHButton = Bool

data FTaskConfig m = TaskConfig
  { mTimeout :: m TimeoutDuration,
    mStyle :: m TreeStyle,
    mIncTask :: m IncludeTask,
    mIncHidden :: m IncludeHidden,
    mListMatch :: m AllowListMatching,
    mSWISHButton :: m ShowSWISHButton,
    specifications :: [Spec]
  }

data Spec = Spec Visibility Visualize Expection Timeout Requirement
  deriving (Show)

data Visibility = Hidden String | Visible
  deriving (Show)

data Visualize = ShowTree | DontShowTree
  deriving (Show)

data Expection = PositiveResult | NegativeResult
  deriving (Show)

data Timeout = GlobalTimeout | LocalTimeout Int
  deriving (Show)

data Requirement
  = StatementToCheck [Term]
  | QueryWithAnswers [Term] [[Term]]
  | NewPredDecl Term String
  deriving (Show)
