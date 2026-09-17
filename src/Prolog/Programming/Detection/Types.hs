{-# OPTIONS_GHC -Wno-orphans #-}

module Prolog.Programming.Detection.Types where

import Language.Prolog (Clause (..))

instance Eq Clause where
  Clause ls1 rs1 == Clause ls2 rs2 = ls1 == ls2 && rs1 == rs2
  _ == _ = False

data ProblemType
  = NoUnusedVariables
  | RestrictCutUsage
  deriving (Show, Eq)

data Problem = Problem
  { problemType :: ProblemType,
    problemClause :: Clause,
    hint :: Maybe String
  }
  deriving (Show, Eq)

data Rule = Rule
  { detectProblems :: [Clause] -> [Problem]
  }

data DetectionConfig = DetectionConfig
  {
  }
