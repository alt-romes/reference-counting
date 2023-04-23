{-# LANGUAGE UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments, ImpredicativeTypes #-}
-- | Simple reference counting with linear types inspired by Advanced Topics in
-- Types and Programming Languages Chapter 1
module Data.Counted
  ( RefC'
  , new
  , share
  , get

  , Data.Counted.forget

  , useM

  , modify
  , modifyM
  , modify'
  -- , refcoerce
  , Counted(..)
  ) where

import Data.Coerce
-- import qualified System.IO.Resource.Linear as Linear
import Control.Functor.Linear as Linear hiding (get, modify)
import Prelude.Linear
import qualified Control.Concurrent.Counter as Counter
import qualified Unsafe.Linear as Unsafe

import Data.Functor.Identity

import Control.Monad.IO.Class.Linear

import Data.Counted.Internal
import qualified Data.Counted.Unsafe as Unsafe.Counted

-- TODO: Move to own package, like StateVar but linear?
-- class Reference ref where
--   refget :: Monad.IO lm => ref a ⊸ lm a
  -- modify :: a ⊸ ref () ⊸ ref a

-- instance Reference Identity where
--   refget (Identity x) = x

-- TODO: This is already using atomic-counter, but this is not good enough.
-- Check out the TODO file.

-- TODO: Probably we want RefC to be parametrised over the linear monad, and require that users define a type synonym, like type RefC = RefC' Renderer


-- TOOD: IT'S CRUCIAL WE MAKE SURE STRUCTURES WITH SUB-REF-COUNTED STRUCTURES
-- ARE UPDATED ON SHARE
-- For now, the invariant is reference counted structures CANNOT contain other reference counted structures.

new :: MonadIO lm -- , Reference ref)
    => Counted a
    => (a ⊸ lm ())
    -> a
    ⊸ lm (RefC' lm a)
new freeC x = Linear.do
  Ur c <- liftSystemIOU $ Counter.new 1
  -- Ur refX <- Unsafe.toLinear newIORef x
  pure $ RefCounted freeC c x

share :: MonadIO lm => RefC' lm a ⊸ lm (RefC' lm a, RefC' lm a)
share = Unsafe.toLinear $ \rc@(RefCounted _ counter x) -> Linear.do
  Ur _ <- liftSystemIOU (Counter.add counter 1) -- increment reference count

  let cfs = countedFields x

  traverse' (Unsafe.toLinear $ \y -> Unsafe.Counted.inc y) cfs >>=
    Unsafe.toLinear (\_ -> -- We can forget the references since we were just unsafely incrementing them all

      -- It's safe to return two references to the pointer because we've
      -- incremented the reference count of this and and all reference counted
      -- fields. Both references must be used linearly *and* we decrement the
      -- reference count with every use except for the last through @free@,
      -- ensuring the resource is freed exactly one.
      pure (rc, rc))

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
-- Usage:
-- @
-- (x, cleanup) <- Counted.get ref
-- x' <- useSomehow x
-- cleanup x'
-- @
get :: MonadIO lm => RefC' lm a ⊸ lm (a, a ⊸ lm ())
get (RefCounted freeC counter x) = Linear.do
  Ur oldCount <- liftSystemIOU (Counter.sub counter 1)
  if oldCount == 1
     then Linear.do
       -- This is the last reference to the resource, free it.
       pure (x, freeC)
     else
      -- This is not the last reference, do nothing else.
      pure (x, Unsafe.toLinear2 const (pure ()))

-- Forget the existence of a linear resource, freeing it if necessary
forget :: MonadIO lm => RefC' lm a ⊸ lm ()
forget (RefCounted freeC counter x) = Linear.do
  Ur _ <- liftSystemIOU (Counter.sub counter 1)
  freeC x

-- refcoerce :: Coercible a b => RefC a ⊸ RefC b
-- refcoerce (RefCounted freeC counter x) = RefCounted (freeC . lcoerce) counter (lcoerce x)

modify :: (a ⊸ a) ⊸ RefC' lm a ⊸ RefC' lm a
modify f (RefCounted freeC counter x) = RefCounted freeC counter (f x)

modifyM :: MonadIO lm => (a ⊸ lm a) ⊸ RefC' lm a ⊸ lm (RefC' lm a)
modifyM f (RefCounted freeC counter x) = RefCounted freeC counter <$> f x

modify' :: Counted b => MonadIO lm => (b ⊸ lm ()) -> (a ⊸ b) ⊸ RefC' lm a ⊸ RefC' lm b
modify' freeC f (RefCounted _ counter x) = RefCounted freeC counter (f x)

-- | Use a reference counted value in an action that uses that value linearly
-- without destroying it.
--
-- The value with the same reference count will be returned together with a
-- byproduct of the linear computation.
useM :: MonadIO m
     => RefC' m a ⊸ (a ⊸ m (a, b)) ⊸ m (RefC' m a, b)
useM (RefCounted freeC counter x) f = f x >>= \(a,b) -> pure (RefCounted freeC counter a, b)

lcoerce :: Coercible a b => a ⊸ b
lcoerce = Unsafe.toLinear coerce

-- TODO: What about refcounted data structures including other refcounted structures? Wouldn't we need to update all children +1? ...


