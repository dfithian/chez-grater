module Chez.Grater.Internal.Json where

import Chez.Grater.Internal.Prelude

import Data.Aeson.TH
  ( Options, allNullaryToStringTag, constructorTagModifier, defaultOptions, fieldLabelModifier
  , omitNothingFields
  )
import Data.Char (toLower)
import Data.List (stripPrefix)

lowerFirst :: String -> String
lowerFirst ((toLower -> c) : cs) = c : cs
lowerFirst cs = cs

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier     = \field -> lowerFirst . fromMaybe field . stripPrefix prefix $ field
  , constructorTagModifier = lowerFirst
  , allNullaryToStringTag  = True
  , omitNothingFields      = True
  }
