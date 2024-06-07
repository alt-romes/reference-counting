-- | This module is designed to be imported qualified. It provides unsafe
-- operations over the reference counted structures in order to build new
-- primitives wrt linearity.
--
-- @
-- import qualified Data.Counted.Unsafe as Unsafe
-- @
{-# LANGUAGE LinearTypes, NoImplicitPrelude, QualifiedDo, UnicodeSyntax,
   BlockArguments #-}
module Data.Linear.Alias.Unsafe where

import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear

import qualified Control.Concurrent.Counter as Counter

import Data.Linear.Alias.Internal

import qualified Unsafe.Linear as Unsafe

-- | Unsafely increment the counter of some reference counted resource
inc :: MonadIO m => Alias m' a ⊸ m (Alias m' a)
inc = Unsafe.toLinear \(Alias f counter a) -> Linear.do
  Ur _ <- incCounter counter -- increment reference count
  pure (Alias f counter a)
  where
    incCounter c = liftSystemIOU (Counter.add c 1)

-- | Unsafely decrement the counter of some reference counted resource and get
-- the reference counted value (it's really, really quite unsafe).
--
-- This doesn't free the resource if the reference count reaches 0.
dec :: MonadIO m => Alias μ a ⊸ m (Ur a)
dec = Unsafe.toLinear \(Alias _ counter a) -> Linear.do
  Ur _ <- decCounter counter -- decrement reference count
  pure (Ur a)
  where
    decCounter c = liftSystemIOU (Counter.sub c 1)

-- | Unsafely get an aliased value. All counters are kept unchanged.
get :: Alias m' a -> a
get (Alias _ _ a) = a

