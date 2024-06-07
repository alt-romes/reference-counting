{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ImpredicativeTypes, UnicodeSyntax, LinearTypes, QualifiedDo,
   NoImplicitPrelude, BlockArguments, DefaultSignatures, QuantifiedConstraints,
   UndecidableInstances, AllowAmbiguousTypes #-}
module Data.Linear.Alias.Internal where

import GHC.Generics

import Prelude.Linear
import qualified Control.Concurrent.Counter as Counter

-- Usage: type RefC = RefC' MonadYou'reUsing

-- | A reference counted alias
data Alias m a where
  Alias :: (a ⊸ m ())       -- ^ Function to free resource when the last alias is forgotten
        -> !Counter.Counter -- ^ The counter associated to this reference counted alias
        -> a                -- ^ The aliased resource
         ⊸ Alias m a

-- I don't see a way to instance Functor if the free resource function is kept
-- in the body of the reference (it shows up in both the co- and contra-variant
-- position)

-- Generic utilities

-- | Return all reference counted (recursively nested) fields of @a@.
-- These are not only @'Alias'es@ directly, but all the recursively nested
-- @'Alias'es@ in @a@.
countedFields :: (Generic a, Fields (Rep a)) => a -> [SomeAlias]
-- The alternative of using the 'Data' constraint instead of Generic isn't
-- good, since it would require making @Alias m a@ instance Data, which is
-- hard.
countedFields x = fields (from x)
{-# INLINE countedFields #-}

data SomeAlias = ∀ m b. SomeAlias (Alias m b)

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

