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


-- | Unsafely increment the counter of some reference counted resource, and of
-- all recursively nested reference counted resources.
inc :: MonadIO m => Alias m' a ⊸ m (Alias m' a)
inc = Unsafe.toLinear \(Alias f counter a) -> Linear.do
  Ur _ <- incCounter counter -- increment reference count

  -- We can forget the counted fields after incrementing them all
  consume <$>
    traverse' (Unsafe.toLinear \(SomeAlias (Alias _ counter' _)) ->
                Linear.do Ur _ <- incCounter counter'; pure ()) (countedFields a)

  pure (Alias f counter a)
  where
    incCounter c = liftSystemIOU (Counter.add c 1)

-- | Unsafely decrement the counter of some reference counted resource and all
-- of its recursively nested reference counted fields, and get the reference
-- counted value (it's really, really quite unsafe).
--
-- This doesn't free the resource if the reference count reaches 0.
--
-- Again, be really careful about it recursively decrementing every nested
-- alias -- the aliases in the returned unrestricted value shouldn't be freed
-- again (hence the unrestrictedness, it re-enforces this is dangerous and that
-- those extra aliases should be ignored, not forgotten.)
--
-- You might be looking for 'get' and 'forget'
dec :: (MonadIO m, Aliasable a) => Alias μ a ⊸ m (Ur a)
dec = Unsafe.toLinear \(Alias _ counter a) -> Linear.do
  Ur _ <- decCounter counter -- decrement reference count

  -- We can forget the counted fields after decrementing them all
  consume <$>
    traverse' (Unsafe.toLinear \(SomeAlias (Alias _ counter' _)) ->
                Linear.do Ur _ <- decCounter counter'; pure ()) (countedFields a)
  pure (Ur a)
  where
    decCounter c = liftSystemIOU (Counter.sub c 1)

-- | Unsafely get an aliased value. All counters are kept unchanged.
get :: Alias m' a -> a
get (Alias _ _ a) = a

