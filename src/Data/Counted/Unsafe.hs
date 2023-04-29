-- | This module is designed to be imported qualified. It provides unsafe
-- operations over the reference counted structures in order to build new
-- primitives wrt linearity.
--
-- @
-- import qualified Data.Counted.Unsafe as Unsafe
-- @
{-# LANGUAGE LinearTypes, NoImplicitPrelude, QualifiedDo, UnicodeSyntax #-}
module Data.Counted.Unsafe where

import Control.Functor.Linear as Linear
import Prelude.Linear
import qualified Control.Concurrent.Counter as Counter

import Control.Monad.IO.Class.Linear

-- import Data.Counted
import Data.Counted.Internal


-- ROMES:TODO: Should inc also increase refcount of all nested refc?
-- If yes, change the `share` definition too.
-- | Unsafely increment the counter of some reference counted structure.
inc :: MonadIO m => RefC' m' a ⊸ m (RefC' m' a)
inc (RefCounted f counter a) = Linear.do
  Ur _ <- liftSystemIOU (Counter.add counter 1) -- increment reference count
  pure (RefCounted f counter a)

