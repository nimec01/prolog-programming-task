module Prolog.Programming.Linting.Helper where

import Language.Prolog (Clause (..), Term (..), VariableName (..))

definesSamePredicate :: Clause -> Clause -> Bool
definesSamePredicate (Clause (Struct a1 _) _) (Clause (Struct a2 _) _) = a1 == a2
definesSamePredicate _ _ = False

termContainsCut :: Term -> Bool
termContainsCut (Cut _) = True
termContainsCut (Struct p args) | p `elem` [",", ";"] = any termContainsCut args
termContainsCut _ = False

namedVariablesInTerm :: Term -> [String]
namedVariablesInTerm (Struct _ args) = concatMap namedVariablesInTerm args
namedVariablesInTerm (Var (VariableName _ a)) = [a]
namedVariablesInTerm _ = []
