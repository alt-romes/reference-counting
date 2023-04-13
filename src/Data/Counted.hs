{-# LANGUAGE DeriveFunctor, UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments #-}
-- | Simple reference counting with linear types as described in Advanced
-- Topics in Types and Programming Languages Chapter 1
module Data.Counted
  ( RefC
  , new
  , share
  , get

  , modify
  , modifyM
  , modify'
  , refcoerce
  ) where

import Data.Coerce
-- import qualified System.IO.Resource.Linear as Linear
import Control.Functor.Linear as Linear hiding (get, modify)
import qualified Data.Functor.Linear as Data
import Prelude.Linear
import qualified Control.Concurrent.Counter as Counter
import System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

import Control.Monad.IO.Class.Linear

-- TODO: This is already using atomic-counter, but this is not good enough.
-- Check out the TODO file.

-- TODO: Interface with Linear.MonadIO

-- | A reference counted value
data RefC a where
  -- TODO: Be sure to use atomic w IORef
  RefCounted :: (∀ lm. MonadIO lm => a ⊸ lm ()) -- ^ Function to free resource
             -> !Counter.Counter  -- ^ The counter associated to this counted reference
             -- ⊸ !(IORef a)        -- ^ The actual reference to the value
             -> a                    -- ^ The actual value
             ⊸ RefC a

-- instance Prelude.Functor RefC where
--   fmap f (RefCounted freeC c x) = (RefCounted (freeC . f) c (f x))

-- TODO: Probably we want RefC to be parametrised over the linear monad, and require that users define a type synonym

new :: MonadIO lm
    => (∀ lm'. MonadIO lm' => a ⊸ lm' ())
    -> a
    ⊸ lm (RefC a)
new freeC x = Linear.do
  Ur c <- liftSystemIOU $ Counter.new 1
  -- Ur refX <- Unsafe.toLinear newIORef x
  pure $ RefCounted freeC c x

share :: MonadIO lm => RefC a ⊸ lm (RefC a, RefC a)
share = Unsafe.toLinear $ \rc@(RefCounted _ counter _) -> Linear.do
  Ur _ <- liftSystemIOU (Counter.add counter 1) -- increment reference count
  -- It's safe to return two references to the pointer because we've
  -- incremented the reference count. Both references must be used linearly
  -- *and* we decrement the reference count with every use except for the last
  -- through @free@, ensuring the resource is freed exactly one.
  pure (rc, rc)

-- | This function returns a value that is reference counted in a linear pair
-- with a function to free the linear value. Since both the value and freeing
-- function must be consumed linearly, it is guaranteed that the returned
-- function is the one used to free the resource.
--
-- The freeing function can be one of two things:
-- * If the returned value was the last reference, the function is the one
-- passed as an argument to 'new'
--
-- * Otherwise, if this isn't the last reference to the value, the freeing
-- function will be a no-op, but still must be called on the value.
--
get :: MonadIO lm => RefC a ⊸ lm (a, a ⊸ lm ())
get (RefCounted freeC counter x) = Linear.do
  Ur oldCount <- liftSystemIOU (Counter.sub counter 1)
  if oldCount == 1
     then Linear.do
       -- This is the last reference to the resource, free it.
       pure (x, freeC)
     else
      -- This is not the last reference, do nothing else.
      pure (x, Unsafe.toLinear2 const (pure ()))


refcoerce :: Coercible a b => RefC a ⊸ RefC b
refcoerce (RefCounted freeC counter x) = RefCounted (freeC . lcoerce) counter (lcoerce x)

modify :: (a ⊸ a) ⊸ RefC a ⊸ RefC a
modify f (RefCounted freeC counter x) = RefCounted freeC counter (f x)

modifyM :: MonadIO lm => (a ⊸ lm a) ⊸ RefC a ⊸ lm (RefC a)
modifyM f (RefCounted freeC counter x) = RefCounted freeC counter <$> f x

modify' :: (∀ lm. MonadIO lm => b ⊸ lm ()) -> (a ⊸ b) ⊸ RefC a ⊸ RefC b
modify' freeC f (RefCounted _ counter x) = RefCounted freeC counter (f x)

lcoerce :: Coercible a b => a ⊸ b
lcoerce = Unsafe.toLinear coerce

-- TODO Interface over Linear.MonadIO


