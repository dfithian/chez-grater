{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chez.Grater.Internal.CI.Orphans where

import Chez.Grater.Internal.Prelude

import Data.Aeson
  ( FromJSON, FromJSONKey, ToJSON, ToJSONKey, fromJSONKey, fromJSONKeyList, parseJSON, toJSON
  , toJSONKey, toJSONKeyList
  )
import Data.Functor.Contravariant (contramap)
import qualified Data.CaseInsensitive as CI

instance (CI.FoldCase a, FromJSON a) => FromJSON (CI a) where
  parseJSON = fmap CI.mk . parseJSON

instance (CI.FoldCase a, FromJSONKey a) => FromJSONKey (CI a) where
  fromJSONKey = CI.mk <$> fromJSONKey
  fromJSONKeyList = fmap CI.mk <$> fromJSONKeyList

instance (ToJSON a) => ToJSON (CI a) where
  toJSON = toJSON . CI.original

instance (ToJSONKey a) => ToJSONKey (CI a) where
  toJSONKey = contramap CI.original toJSONKey
  toJSONKeyList = contramap (fmap CI.original) toJSONKeyList
