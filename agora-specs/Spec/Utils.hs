{- |
Module     : Spec.Utils
Maintainer : emi@haskell.fyi
Description: Tests for utility functions in 'Agora.Utils'.

Tests for utility functions in 'Agora.Utils'.
-}
module Spec.Utils (tests) where

import Property.Utils qualified as Props
import Test.Tasty (TestTree, testGroup)

tests :: [TestTree]
tests = [testGroup "properties" Props.props]
