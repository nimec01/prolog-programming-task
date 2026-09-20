module Prolog.Programming.CodeAnalysis.Helper where

import Language.Prolog (Clause (..), Term (..))

definesSamePredicate :: Clause -> Clause -> Bool
definesSamePredicate (Clause (Struct a1 args1) _) (Clause (Struct a2 args2) _) = a1 == a2 && length args1 == length args2
definesSamePredicate _ _ = False

termContainsCut :: Term -> Bool
termContainsCut (Cut _) = True
termContainsCut (Struct p args) | p `elem` [",", ";"] = any termContainsCut args
termContainsCut _ = False
