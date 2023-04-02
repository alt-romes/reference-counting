{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Atomic.Int
  ( MutInt,
    newMutInt,
    getMutInt,
    putMutInt,
  )
where

import GHC.Exts
import GHC.Types

newtype MutInt = MutInt Any

newMutInt :: IO MutInt
newMutInt = IO $ \s0 -> case stg_newMutInt# s0 of
  (# s1, c #) -> (# s1, MutInt c #)

getMutInt :: MutInt -> IO Int
getMutInt (MutInt c) = IO $ \s0 -> case stg_getMutInt# s0 c of
  (# s1, x #) -> (# s1, I# x #)

putMutInt :: MutInt -> Int -> IO ()
putMutInt (MutInt c) (I# x) = IO $ \s0 -> case stg_putMutInt# s0 c x of
  (# s1 #) -> (# s1, () #)

-- TODO: fetchAddWordAddr#

foreign import prim "stg_newMutIntzh"
  stg_newMutInt# :: State# s -> (# State# s, Any #)

foreign import prim "stg_getMutIntzh"
  stg_getMutInt# :: State# s -> Any -> (# State# s, Int# #)

foreign import prim "stg_putMutIntzh"
  stg_putMutInt# :: State# s -> Any -> Int# -> (# State# s #)

