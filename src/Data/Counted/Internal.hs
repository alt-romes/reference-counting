-- | TODO Hide this module in haddock, or remove from exposed-modules altogether
{-# LANGUAGE ImpredicativeTypes, UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments #-}
module Data.Counted.Internal where

import qualified Prelude
import Prelude.Linear
import qualified Data.IntMap as IM
import qualified Control.Concurrent.Counter as Counter
import Control.Monad.IO.Class.Linear

-- Usage: type RefC = RefC' MonadYou'reUsing

-- | A reference counted value
data RefC' lm a where
  -- TODO: Allow custom references (ensure they're atomically modified through
  -- a class 'Reference' or smtg)
  RefCounted :: Counted a -- Reference ref
             => (a ⊸ lm ()) -- ^ Function to free resource
             -> !Counter.Counter  -- ^ The counter associated to this counted reference
             -- ⊸ !(IORef a)        -- ^ The actual reference to the value
             -> a  -- ref a          -- ^ The actual value
             ⊸ RefC' lm a

-- instance Prelude.Functor RefC where
--   fmap f (RefCounted freeC c x) = (RefCounted (freeC . f) c (f x))

data SomeRefC = forall m b. SomeRefC (RefC' m b)

class Counted a where
  -- | Must return all reference counted (recursively nested) fields of @a@.
  -- This is not only the direct 'RefC', but also the nested 'RefC's of all
  -- fields that instance 'Counted'.
  -- If you fail to implement this correctly, reference counting won't be sound!
  countedFields :: a -> [SomeRefC]

instance (Counted a, Counted b) => Counted (a,b) where
  countedFields (x,y) = countedFields x ++ countedFields y

instance Counted a => Counted (IM.IntMap a) where
  countedFields = Prelude.foldMap countedFields

