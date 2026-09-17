module Prolog.Programming.Detection.Helper where

import Language.Prolog (Clause (..), Term (..))

definesSamePredicate :: Clause -> Clause -> Bool
definesSamePredicate (Clause (Struct a1 _) _) (Clause (Struct a2 _) _) = a1 == a2
definesSamePredicate _ _ = False

termIsCut :: Term -> Bool
termIsCut (Cut _) = True
termIsCut _ = False
