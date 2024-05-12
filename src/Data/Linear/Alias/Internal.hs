{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ImpredicativeTypes, UnicodeSyntax, LinearTypes, QualifiedDo,
   NoImplicitPrelude, BlockArguments, DefaultSignatures, QuantifiedConstraints,
   UndecidableInstances, AllowAmbiguousTypes #-}
module Data.Linear.Alias.Internal where

import GHC.Generics

import Prelude.Linear
import qualified Control.Concurrent.Counter as Counter
import qualified Prelude

-- containers, for instances
import qualified Data.IntMap as IM
import qualified Data.Map as M

-- Usage: type RefC = RefC' MonadYou'reUsing

-- | A reference counted alias
data Alias m a where
  Alias :: Aliasable a
        => (a ⊸ m ())       -- ^ Function to free resource when the last alias is forgotten
        -> !Counter.Counter -- ^ The counter associated to this reference counted alias
        -> a                -- ^ The aliased resource
         ⊸ Alias m a

-- I don't see a way to instance Functor if the free resource function is kept
-- in the body of the reference

data SomeAlias = ∀ m b. SomeAlias (Alias m b)
class Aliasable a where

  -- | Must return all reference counted (recursively nested) fields of @a@.
  -- These are not only the direct @'Alias'es@, but also the nested @'Alias'es@ of
  -- all fields that instance @'Aliasable'@.
  --
  -- If you fail to implement this correctly, reference counting won't be sound!
  -- Good thing is we can do this automatically! It's much less bug-prone,
  -- especially if you update the definition of a datatype. Your @a@ just needs
  -- to instance 'Generic'.
  countedFields :: a -> [SomeAlias]

  default countedFields :: (Generic a, Fields (Rep a)) => a -> [SomeAlias]
  -- The alternative of using the 'Data' constraint instead of Generic isn't
  -- good, since it would require making @Alias m a@ instance Data, which is
  -- hard.
  countedFields x = fields (from x)
  {-# INLINE countedFields #-}

instance (Aliasable a, Aliasable b) => Aliasable (a,b) where
  countedFields (x,y) = countedFields x ++ countedFields y
  {-# INLINE countedFields #-}

instance Aliasable a => Aliasable [a] where
  countedFields = Prelude.foldMap countedFields
  {-# INLINE countedFields #-}

instance Aliasable a => Aliasable (IM.IntMap a) where
  countedFields = Prelude.foldMap countedFields
  {-# INLINE countedFields #-}

instance (Aliasable k, Aliasable v) => Aliasable (M.Map k v) where
  countedFields m = countedFields (M.toList m)
  {-# INLINE countedFields #-}

-- | One ought to be careful about the instance of 'Aliasable' for 'Ur' things.
-- We simply ignore all possibly reference counted values in the structure
-- underlying 'Ur' -- the reasoning being there's little value in reference
-- counting an unrestricted value (there's nothing enforcing correct counting).
instance Aliasable (Ur a) where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable () where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable Int where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable Word where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable Float where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable Double where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable Integer where
  countedFields _ = []
  {-# INLINE countedFields #-}

instance Aliasable Rational where
  countedFields _ = []
  {-# INLINE countedFields #-}

-- Generic utilities

instance (Generic a, Fields (Rep a)) => Aliasable (Generically a) where
  countedFields (Generically a) = fields (from a)

class Fields rep where
  fields :: rep a -> [SomeAlias]

instance Fields V1 where
  fields _ = []

instance Fields U1 where
  fields _ = []

instance Fields f => Fields (M1 i c f) where
  fields (M1 x) = fields x

instance (Fields a, Fields b) => Fields (a :+: b) where
  fields (L1 x) = fields x
  fields (R1 x) = fields x

instance (Fields a, Fields b) => Fields (a :*: b) where
  fields (a :*: b) = fields a ++ fields b

-- In the case of an alias, we add it the list of aliased values.
instance Fields (K1 i (Alias m a)) where
  fields (K1 r) = [SomeAlias r]

-- In the case it's some unknown aliasable structure, we recurse.
instance Aliasable c => Fields (K1 i c) where
  fields (K1 r) = countedFields r

