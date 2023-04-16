-- | TODO Hide this module in haddock, or remove from exposed-modules altogether
{-# LANGUAGE UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments #-}
module Data.Counted.Internal where

import qualified Control.Concurrent.Counter as Counter
import Control.Monad.IO.Class.Linear

-- | A reference counted value
data RefC a where
  -- TODO: Allow custom references (ensure they're atomically modified through
  -- a class 'Reference' or smtg)
  RefCounted :: -- Reference ref
                (∀ lm. MonadIO lm => a ⊸ lm ()) -- ^ Function to free resource
             -> !Counter.Counter  -- ^ The counter associated to this counted reference
             -- ⊸ !(IORef a)        -- ^ The actual reference to the value
             -> a  -- ref a          -- ^ The actual value
             ⊸ RefC a

-- instance Prelude.Functor RefC where
--   fmap f (RefCounted freeC c x) = (RefCounted (freeC . f) c (f x))


