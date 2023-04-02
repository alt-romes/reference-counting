{-# LANGUAGE LinearTypes, QualifiedDo #-}
module Main (main) where

import Data.Counted
import System.IO.Linear as Linear
import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear

main :: Prelude.IO ()
main = withLinearIO $ Linear.do
  ref <- new undefined 'a'
  (ref1, ref2) <- share ref
  free ref1
  free ref2
  pure (Ur ())

