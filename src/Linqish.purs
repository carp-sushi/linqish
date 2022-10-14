module Linqish where

import Prelude

import Control.Alternative (class Alternative, guard)
import Data.Tuple (Tuple(..))

-- Extract fields from a row.
select_ :: forall f a b. Functor f => (a -> b) -> f a -> f b
select_ = map

-- Filter rows based on a predicate.
where_ :: forall m a. Bind m => Alternative m => (a -> Boolean) -> m a -> m a
where_ fn ma = do
  a <- ma
  guard $ fn a
  pure a

-- Combine two rows when they have a value in common.
join_ :: forall m a b c. Bind m => Alternative m => Eq c => m a -> (a -> c) -> m b -> (b -> c) -> m (Tuple a b)
join_ ma f mb g = do
  a <- ma
  b <- mb
  guard $ f a == g b
  pure $ Tuple a b

-- Describes how to run a Linqish query.
linqish_ :: forall a b c. (b -> c) -> a -> (a -> b) -> c
linqish_ s j w = (s <<< w) j

-- A wrapper that allows for specifying a query without a where clause.
data Query :: forall k. (k -> Type) -> k -> k -> Type
data Query m a b
  = Query (m a -> m b) (m a) (m a -> m a)
  | Query_ (m a -> m b) (m a)

-- Runs Linqish queries.
runQuery :: forall m a b. Bind m => Alternative m => Query m a b -> m b
runQuery (Query s j w) = linqish_ s j w
runQuery (Query_ s j) = linqish_ s j (where_ $ const true)
