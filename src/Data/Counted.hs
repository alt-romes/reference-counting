{-# LANGUAGE UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude #-}
-- | Simple reference counting with linear types as described in Advanced
-- Topics in Types and Programming Languages Chapter 1
module Data.Counted
  ( RefC
  , new
  , share
  , free
  , set
  , modify
  , get
  ) where

-- import qualified System.IO.Resource.Linear as Linear
import Data.IORef (IORef)
import qualified Data.IORef
import Control.Functor.Linear as Linear hiding (get, modify)
import Prelude.Linear
import qualified Prelude
import qualified Control.Concurrent.Counter as Counter
import System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

-- TODO: This is already using atomic-counter, but this is not good enough.
-- Check out the TODO file.

-- | A reference counted value
data RefC a where
  -- TODO: Be sure to use atomic w IORef
  RefCounted :: (a ⊸ Linear.IO ()) -- ^ Function to free resource
             ⊸ !Counter.Counter  -- ^ The counter associated to this counted reference
             ⊸ !(IORef a)        -- ^ The actual reference to the value
             ⊸ RefC a

new :: (a ⊸ Linear.IO ())
    ⊸ a
    ⊸ Linear.IO (RefC a)
new freeC x = Linear.do
  Ur c <- fromSystemIOU $ Counter.new 1
  Ur refX <- Unsafe.toLinear newIORef x
  pure $ RefCounted freeC c refX


share :: RefC a ⊸ Linear.IO (RefC a, RefC a)
share = Unsafe.toLinear $ \rc@(RefCounted _ counter _) -> Linear.do
  Ur _ <- fromSystemIOU $ (Counter.add counter 1) -- increment reference count
  -- It's safe to return two references to the pointer because we've
  -- incremented the reference count. Both references must be used linearly
  -- *and* we decrement the reference count with every use except for the last
  -- through @free@, ensuring the resource is freed exactly one.
  pure (rc, rc)

free :: RefC a ⊸ Linear.IO ()
free = Unsafe.toLinear $ \(RefCounted freeC counter refX) -> Linear.do
  Ur oldCount <- fromSystemIOU $ (Counter.sub counter 1)
  if oldCount == 1
     -- This is the last reference to the resource, free it.
     then Linear.do
       Ur x <- Unsafe.toLinear readIORef refX
       freeC x
     else
      -- This is not the last reference, do nothing else.
      pure ()

set :: a ⊸ RefC a ⊸ Linear.IO (RefC a)
set = Unsafe.toLinear2 $ \x (RefCounted freeC counter refX) -> fromSystemIO $ do
  () <- Data.IORef.atomicWriteIORef refX x
  Prelude.pure $ RefCounted freeC counter refX

modify :: (a -> a) ⊸ RefC a ⊸ Linear.IO (RefC a)
modify = Unsafe.toLinear2 $ \f (RefCounted freeC counter refX) -> fromSystemIO $ do
  () <- Data.IORef.atomicModifyIORef' refX ((,()) Prelude.. f)
  Prelude.pure $ RefCounted freeC counter refX

get :: RefC a ⊸ Linear.IO (Ur a)
get = Unsafe.toLinear $ \(RefCounted _ _ refX) -> readIORef refX

