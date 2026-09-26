{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Prolog.Programming.TypeHelper (recordFieldNames) where

import Data.Kind (Type)
import GHC.Generics (
  C,
  D,
  Generic (Rep),
  K1,
  M1,
  S,
  Selector (selName),
  U1,
  type (:*:),
 )

class FieldNames (f :: Type -> Type) where
  fieldNames :: [String]

instance FieldNames U1 where
  fieldNames = []

instance (FieldNames f, Selector s) => FieldNames (M1 S s f) where
  fieldNames =
    selName (undefined :: M1 S s f p)
      : fieldNames @f

instance (FieldNames l, FieldNames r) => FieldNames (l :*: r) where
  fieldNames = fieldNames @l ++ fieldNames @r

instance FieldNames f => FieldNames (M1 C c f) where
  fieldNames = fieldNames @f

instance FieldNames f => FieldNames (M1 D d f) where
  fieldNames = fieldNames @f

instance FieldNames (K1 i c) where
  fieldNames = []

recordFieldNames
  :: forall a. (FieldNames (Rep a), Generic a) => [String]
recordFieldNames = fieldNames @(Rep a)
