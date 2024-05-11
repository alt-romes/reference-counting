{-# LANGUAGE NoImplicitPrelude, LinearTypes, QualifiedDo, DerivingStrategies, DeriveAnyClass, BlockArguments #-}
module Main (main) where

import GHC.Generics
import qualified Prelude
import System.IO.Linear as Linear (withLinearIO, IO, fromSystemIO, fromSystemIOU)
import Prelude.Linear hiding (IO)
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as DL
import qualified Unsafe.Linear as Unsafe

-- reference-counting
import qualified Data.Linear.Alias as A

--------------------------------------------------------------------------------
-- Test 1
--------------------------------------------------------------------------------

data SomeResource where
  UnsafeCreateResource :: Int -> SomeResource

  deriving Generic
  deriving anyclass A.Aliasable

newResource :: Int %1 -> IO SomeResource
freeResource :: SomeResource %1 -> IO ()

test1 :: IO ()
test1 = Linear.do
  res <- newResource 0
  ref <- A.newAlias freeResource res
  (ref1, ref2) <- A.share ref
  A.forget ref1
  A.forget ref2

--------------------------------------------------------------------------------
-- Test 2
--------------------------------------------------------------------------------

test2 :: IO ()
test2 = Linear.do
  let list = [1..200]
  refs <- DL.forM list \i -> Linear.do
    newResource i >>= A.newAlias freeResource
  case refs of
    [] -> ok
    (x:xs) -> Linear.do
      (x1, x2) <- A.share x
      A.forget x1
      A.forget (x2:xs)
      -- sort | uniq -c of the stdin should have 1 entry for i=1 to i=200 freed
  where
    ok = fromSystemIO $ putStrLn "ok"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: Prelude.IO ()
main = withLinearIO $ Linear.do
  () <- test1
  () <- test2
  pure (Ur ())


--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

{-# OPAQUE newResource #-}
{-# OPAQUE freeResource #-}
newResource = Unsafe.toLinear \i -> Linear.do
  () <- fromSystemIO $ Prelude.putStrLn $ "initialising linear resource " ++ Prelude.show i
  return $ UnsafeCreateResource i
freeResource (UnsafeCreateResource i) = Linear.do
  () <- fromSystemIO $ Prelude.putStrLn $ "freeing linear resource " ++ Prelude.show i
  return ()
