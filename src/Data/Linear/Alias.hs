{-# LANGUAGE UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments, ImpredicativeTypes, DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-} -- Rep a in the Generically instance
-- | Simple reference counting with linear types inspired by Advanced Topics in
-- Types and Programming Languages Chapter 1
module Data.Linear.Alias
  (
  -- * The heart of aliasing
    Alias
  , Shareable(..)
  , Forgettable(..)
  , get

  -- * Using and modifying aliased resources
  , use
  , useM
  , modify
  , modifyM
  , hoist

  -- * Creating aliases
  , newAlias
  ) where

import GHC.Generics
import Control.Functor.Linear as Linear hiding (get, modify)
import Control.Monad.IO.Class.Linear
import Prelude.Linear hiding (forget)
import qualified Control.Concurrent.Counter as Counter
import qualified Unsafe.Linear as Unsafe
import qualified Data.IntMap as IM
import qualified Data.Bifunctor.Linear as B

import Data.Linear.Alias.Internal
import qualified Data.Linear.Alias.Unsafe as Unsafe.Alias

-- TODO: This is already using atomic-counter, but this is not good enough (it would be cool to use the unlifted one).
-- Check out the TODO file.

----- Signature functions -----

-- | Create an alias for a resource.
newAlias :: MonadIO m
    => (a ⊸ μ ()) -- ^ Function to free resource when the last alias is released
     ⊸ a          -- ^ The resource to alias
     ⊸ m (Alias μ a)
newAlias = Unsafe.toLinear \freeC x -> Linear.do
  Ur c <- liftSystemIOU $ Counter.new 1
  pure $ Alias freeC c x

-- | This function returns a value that is aliased in a linear pair with a
-- function to free the linear value. Since both the value and freeing function
-- must be consumed linearly, it is guaranteed that the returned function is
-- the one used to free the resource.
--
-- The cleanup function can be one of two things:
--
-- * If the returned value was the last reference, the function is the one
-- passed as an argument to 'new'
--
-- * Otherwise, if this isn't the last reference to the value, the freeing
-- function will be a no-op, but still must be called on the value.
--
-- Usage:
--
-- @
-- (x, cleanup) <- Alias.get ref
-- x' <- useSomehow x
-- cleanup x'
-- @
get :: MonadIO μ => Alias μ a ⊸ μ (a, a ⊸ μ ())
-- Note on unsafety: If some (sub-)resource is unsafely duplicated from the
-- returned @a@, it's crucial that if @a@ is freed manually, it cannot be
-- passed on to the resource-freeing-function! If you don't mess with unsafety
-- this shouldn't be possible. (This is kind of a moot comment since it must be
-- considered in each case carefully).
get (Alias freeC counter x) = Linear.do
  -- The freeing function, in consuming @x@ linearly, is guaranteed to also
  -- free/forget recursively nested reference-counted resources (guaranteed in
  -- its implementation by linearity @freeC@).
  Ur oldCount <- liftSystemIOU (Counter.sub counter 1)
  if oldCount == 1
     then Linear.do
       -- This is the last reference to the resource, free it.
       pure (x, freeC)
     else
      -- This is not the last reference, do nothing else.
      pure (x, Unsafe.toLinear (\_ -> pure ()))

-- | Like 'modifyM'
modify :: (a ⊸ a) ⊸ Alias μ a ⊸ Alias μ a
modify f (Alias freeC counter x) = Alias freeC counter (f x)

-- | Run a monadic function that modifies a reference counted resource.
modifyM :: MonadIO m => (a ⊸ m a) ⊸ Alias μ a ⊸ m (Alias μ a)
modifyM f (Alias freeC counter x) = Alias freeC counter <$> f x

-- | Like 'useM'
use :: Alias μ a ⊸ (a ⊸ (a, b)) ⊸ (Alias μ a, b)
use (Alias freeC counter x) f = case f x of (a,b) -> (Alias freeC counter a, b)

-- | Use a reference value in an action that uses that value linearly
-- without destroying it.
--
-- The value with the same reference count will be returned together with a
-- byproduct of the linear computation.
useM :: MonadIO m
     => Alias μ a ⊸ (a ⊸ m (a, b)) ⊸ m (Alias μ a, b)
useM (Alias freeC counter x) f = f x >>= \(a,b) -> pure (Alias freeC counter a, b)


hoist :: MonadIO m => ((a ⊸ m ()) ⊸ b ⊸ μ ()) ⊸ (a ⊸ b) ⊸ Alias m a ⊸ Alias μ b
hoist = Unsafe.toLinear \freeAB f (Alias freeA counter x) -> Alias (freeAB freeA) counter (f x)

{-# INLINABLE get #-}

----- Signature classes -----

class Forgettable m a where
  -- | Forget the existence of a linear resource
  forget :: MonadIO m => a ⊸ m ()
  -- romes: I had never thought about it, but it's a bit weird for the method
  -- to have constraints over the @m@, where the instances don't have to
  -- require that instance?

instance Forgettable μ (Alias μ a) where
  -- | Forget the existence of a linearly aliased resource, freeing it if necessary
  forget :: MonadIO μ => Alias μ a ⊸ μ ()
  forget (Alias freeC counter x) = Linear.do
    -- Read comment regarding the freeing function and recursively nested aliases in @'get'@.
    Ur oldCount <- liftSystemIOU (Counter.sub counter 1)
    if oldCount == 1
       then Linear.do
         -- This is the last reference to the resource, free it.
         freeC x
       else
         -- No-op
         pure (Unsafe.toLinear (\_ -> ()) x)
  {-# INLINE forget #-} -- this was showing up in profiles

class Shareable m a where
  -- | Share a linear resource
  --
  -- Careful! You must make sure that all aliases recursively nested within
  -- this structure @a@ are properly shared/incremented. 
  --
  -- If you fail to implement this correctly, reference counting won't be sound.
  -- Good thing is we can do this automatically! It's much less bug-prone,
  -- especially if you update the definition of a datatype. Your @a@ just needs
  -- to instance 'Generic'.
  share :: MonadIO m => a ⊸ m (a, a)

  default share :: (Generic a, Fields (Rep a)) => MonadIO m => a ⊸ m (a, a)
  share = Unsafe.toLinear $ \x -> Linear.do

    -- We can forget the counted fields after incrementing them all
    consume <$>
      traverse' (\(SomeAlias alias) -> Linear.do
        a' <- Unsafe.Alias.inc alias -- increment reference count
        Unsafe.toLinear2 const (pure ()) a') (countedFields x)

    -- It's safe to return two references to the pointer because we've
    -- incremented the reference count of all nested aliases. Both references must be used
    -- linearly *and* we decrement the reference count with every use except
    -- for the last through @free@, ensuring the resource is freed exactly one.
    return (x,x)

instance Shareable m (Alias μ a) where
  -- | Share a linearly aliased resource, the heart of reference counting aliases.
  share :: MonadIO m => Alias μ a ⊸ m (Alias μ a, Alias μ a)
  share alias'' = Linear.do
    -- Implement manually since there is no Generic instance for Alias
    alias' <- Unsafe.Alias.inc alias'' 
    pure $ Unsafe.toLinear (\alias -> (alias, alias)) alias'
  {-# INLINE share #-} -- this was showing up in profiles

instance (Generic a, Fields (Rep a)) => Shareable m (Generically a) where
  share = Unsafe.toLinear $ \(Generically x) -> Linear.do
    -- Same implementation as the default instance of `share`
    consume <$>
      traverse' (\(SomeAlias alias) -> Linear.do
        a' <- Unsafe.Alias.inc alias -- increment reference count
        Unsafe.toLinear2 const (pure ()) a') (countedFields x)
    return (Generically x, Generically x)

-- Just like 'share', but doesn't require the action to be performed whithin
-- a MonadIO. Is there a situation where one might want to use 'share'
-- explicitly rather than dup? At least while let bindings are not working for linear types.
--
-- I suppose we also require that this function is bound strictly, as otherwise the value might not be incremented when expected
-- In that sense I suppose it is unsafe? If we call unsafeDup, then forget, but don't force the result of unsafeDup, forget will actually forget the result instead of having been incremented?
-- Have I taken precautions enough to ensure things happen as expected or does unsafePerformIO still not give me that?
--
-- Still seems unwieldly... the side effects are too observable
-- unsafeDup :: Alias x a ⊸ (Alias x a, Alias x a)
-- unsafeDup s = let !(a1,a2) = Unsafe.toLinear unsafePerformIO (share s) in (a1,a2)
-- {-# NOINLINE unsafeDup #-}

----- Other instances -----

-- The Consumable => Forgettable and Dupable => Shareable instances end up being a nuisance, despite being useful to have forgettable instances of things like Ints for free.

-- instance {-# OVERLAPPABLE #-} Consumable a => Forgettable m a where
--   forget a = pure (consume a)
--   {-# INLINE forget #-}
-- instance {-# OVERLAPPABLE #-} Dupable a => Shareable m a where
--   share a = pure (dup2 a)
--   {-# INLINE share #-}

instance (Forgettable m a, Forgettable m b) => Forgettable m (a,b) where
  forget (a,b) = forget a >> forget b
  {-# INLINE forget #-}

instance (Forgettable m a, Forgettable m b, Forgettable m c) => Forgettable m (a,b,c) where
  forget (a,b,c) = forget a >> forget b >> forget c
  {-# INLINE forget #-}

instance Forgettable m a => Forgettable m (IM.IntMap a) where
  forget im = consume <$> traverse' forget (Unsafe.toLinear IM.elems im)
  {-# INLINE forget #-}

instance Forgettable m a => Forgettable m [a] where
  forget l = consume <$> traverse' forget l
  {-# INLINE forget #-}

instance (Shareable m a, Shareable m b) => Shareable m (a,b) where
  share (a0,b0) = Linear.do
    (a1,a2) <- share a0
    (b1,b2) <- share b0
    pure ((a1,b1),(a2,b2))
  {-# INLINE share #-}

instance Shareable m a => Shareable m (IM.IntMap a) where
  share im = B.bimap (Unsafe.toLinear IM.fromList) (Unsafe.toLinear IM.fromList) . unzip <$>
             traverse' share (Unsafe.toLinear IM.toList im)
  {-# INLINE share #-}

instance Shareable m a => Shareable m [a] where
  share l = unzip <$> traverse' share l
  {-# INLINE share #-}

instance Forgettable m Int where
  forget = pure . consume
  {-# INLINE forget #-}
instance Shareable m Int where
  share = pure . dup2
  {-# INLINE share #-}

