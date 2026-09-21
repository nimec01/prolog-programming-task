{-# LANGUAGE LambdaCase #-}

module Prolog.Programming.CodeAnalysis.Helper where

import Data.Generics (Data, everything, mkQ)
import Language.Prolog (Term (..))

containsCut :: (Data a) => a -> Bool
containsCut = everything (||) $ mkQ False $ \case
  Cut _ -> True
  _ -> False
