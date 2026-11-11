module Prolog.Programming.Mutation.Mutators (
  dropClauseMutator,
  dropGoalMutator,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import Data.List.HT (removeEach)
import Language.Prolog (Atom, Goal, Term (..))
import Prolog.Programming.Mutation.Types (Mutant (..), Mutation (..), Mutator)

-- | Returns all possible programs where one clause is dropped
dropClauseMutator :: Mutator
dropClauseMutator [_] = []
dropClauseMutator prog =
  map
    (\(dropped, newProg) -> Mutant newProg (DropClause dropped))
    $ removeEach prog

-- | Returns all possible programs where exactly one goal is dropped
dropGoalMutator :: Mutator
dropGoalMutator = go id
  where
    go _ [] = []
    go addPrefix (c@(n, a, gs) : cs) =
      [Mutant (addPrefix $ (n, a, goals) : cs) (DropGoal c goal) | (goal, goals) <- removeEachGoal gs]
        ++ go (addPrefix . (c :)) cs

removeEachGoal :: [Goal] -> [(Goal, [Goal])]
removeEachGoal = go id
  where
    go _ [] = []
    go addPrefix (g : gs) =
      map (second (addPrefix . maybe gs (: gs))) (removeWithinGoal g)
        ++ go (addPrefix . (g :)) gs

    removeWithinGoal :: Term -> [(Term, Maybe Term)]
    removeWithinGoal (Struct name [l, r])
      | name `elem` [",", ";"] =
          map (second (\remaining -> combine name remaining (Just r))) (removeWithinGoal l)
            ++ map (second (combine name (Just l))) (removeWithinGoal r)
    removeWithinGoal goal = [(goal, Nothing)]

    combine :: Atom -> Maybe Term -> Maybe Term -> Maybe Term
    combine name (Just left) (Just right) = Just $ Struct name [left, right]
    combine _ left right = left <|> right
