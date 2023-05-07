-- | TODO Hide this module in haddock, or remove from exposed-modules altogether
{-# LANGUAGE ImpredicativeTypes, UnicodeSyntax, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments, DefaultSignatures, QuantifiedConstraints, UndecidableInstances, AllowAmbiguousTypes #-}
module Data.Counted.Internal where

import GHC.Generics
import Type.Reflection
import Data.Kind

import qualified Prelude
import Prelude.Linear
import qualified Data.IntMap as IM
import qualified Control.Concurrent.Counter as Counter
import Control.Monad.IO.Class.Linear

import qualified Data.Maybe (catMaybes)

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

data Test = Test Int (RefC' IO ())
  deriving Generic

-- instance Prelude.Functor RefC where
--   fmap f (RefCounted freeC c x) = (RefCounted (freeC . f) c (f x))

data SomeRefC = forall m b. SomeRefC (RefC' m b)

class Counted a where
  -- | Must return all reference counted (recursively nested) fields of @a@.
  -- This is not only the direct 'RefC', but also the nested 'RefC's of all
  -- fields that instance 'Counted'.
  -- If you fail to implement this correctly, reference counting won't be sound!
  --
  -- We can do this automatically! It's much less bug-prone
  countedFields :: a -> [SomeRefC]
  default countedFields :: (Generic a, Fields (Rep a)) => a -> [SomeRefC]
  -- simpler 'Data' constraint instead of Generic isn't good enough bc requires making RefC' instance Data, which is hard
  countedFields x = fields (from x)

instance (Counted a, Counted b) => Counted (a,b) where
  countedFields (x,y) = countedFields x ++ countedFields y

instance Counted a => Counted (IM.IntMap a) where
  countedFields = Prelude.foldMap countedFields

-- Utils

class Fields rep where
  fields :: rep a -> [SomeRefC]

instance Fields V1 where
  fields _ = []

instance Fields U1 where
  fields _ = []

instance {-# OVERLAPPABLE #-} Fields (K1 i c) where
  fields _ = []

instance {-# OVERLAPPING #-} Fields (K1 i (RefC' x y)) where
  fields (K1 r) = [SomeRefC r]

instance Fields f => Fields (M1 i c f) where
  fields (M1 x) = fields x

instance (Fields a, Fields b) => Fields (a :+: b) where
  fields (L1 x) = fields x
  fields (R1 x) = fields x

instance (Fields a, Fields b) => Fields (a :*: b) where
  fields (a :*: b) = fields a ++ fields b

