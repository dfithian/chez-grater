module Chez.Grater.Internal.Prelude
  ( module Prelude
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Exception
  , module Control.Monad
  , module Data.CaseInsensitive
  , module Data.Foldable
  , module Data.HashMap.Strict
  , module Data.List
  , module Data.Map.Strict
  , module Data.Maybe
  , module Data.Set
  , module Data.Text
  , module Data.Traversable
  , nubOrd, uncurry3, uncurry4, tshow, headMay, lastMay
  ) where

import Control.Applicative ((<|>), optional)
import Control.Arrow (first, left, right, second)
import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM, void)
import Data.CaseInsensitive (CI)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList, traverse_)
import Data.HashMap.Strict (HashMap)
import Data.List (find, groupBy, sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Prelude
import qualified Data.Text as Text

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w

tshow :: Show a => a -> Text
tshow = Text.pack . show

headMay :: [a] -> Maybe a
headMay = \case
  x:_ -> Just x
  [] -> Nothing

lastMay :: [a] -> Maybe a
lastMay = headMay . reverse
