module Prolog.Programming.CodeAnalysis.Rules.SingletonVariables (singletonVariablesRule) where

import Data.Generics (Data, everything, mkQ)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map (empty, foldrWithKey, singleton, unionWith)
import Data.Text.Lazy (pack)
import Language.Prolog (Clause (..), Term (..), VariableName (..))
import Prolog.Programming.CodeAnalysis.Types (Problem (..), Rule)
import Text.PrettyPrint.Leijen.Text (Doc, indent, linebreak, string, vsep)

singletonVariablesRule :: Rule
singletonVariablesRule clause =
  map
    ( \var ->
        toProblem
          clause
          [ string (pack $ "includes the singleton variable " ++ var ++ ".") <> linebreak,
            string $ pack "You can safely replace it with a wildcard (_)."
          ]
    )
    shouldBeWildcard
    ++ map
      ( \var ->
          toProblem
            clause
            [ string
                ( pack $
                    "includes singleton-marked variable "
                      ++ var
                      ++ " that is used more than once."
                )
                <> linebreak,
              string $ pack "You should remove the underscore prefix."
            ]
      )
      singletonMarkedMultiple
  where
    variableCounts = countVariables clause
    (shouldBeWildcard, singletonMarkedMultiple) =
      Map.foldrWithKey
        update
        ([], [])
        variableCounts

    update k v (a, b) = case ("_" `isPrefixOf` k, v) of
      (False, 1) -> (k : a, b)
      (True, v') | v' >= 2 -> (a, k : b)
      _ -> (a, b)

countVariables :: (Data a) => a -> Map String Int
countVariables = everything (Map.unionWith (+)) $ mkQ Map.empty count
  where
    count :: Term -> Map String Int
    count (Var (VariableName _ name)) = Map.singleton name 1
    count _ = Map.empty

toProblem :: Clause -> [Doc] -> Problem
toProblem clause desc =
  Problem
    { problemClause = clause,
      problemDisplay =
        vsep $
          [ string $ pack "Your clause",
            indent 2 $ string $ pack $ show clause
          ]
            ++ desc
    }
