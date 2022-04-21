{- |
Module     : Spec.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module tests the Treasury Withdrawal Effect.
-}
module Spec.Effect.TreasuryWithdrawal (currSymbol, signer, validator, validatorHashTN, scriptContext1, tests) where

import Spec.Sample.Effect.TreasuryWithdrawal


import Agora.Effect.TreasuryWithdrawal

import Spec.Util

import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "effect"
      [effectSucceedsWith "test1" (treasuryWithdrawalValidator currSymbol) datum scriptContext1]
  ]
