{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Stake
Maintainer : emi@haskell.fyi
Description: Tests for Stake policy and validator

Tests for Stake policy and validator
-}
module Spec.Stake (tests) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

import Agora.Stake (StakeDatum (StakeDatum), StakeRedeemer (DepositWithdraw), stakePolicy, stakeValidator)

--------------------------------------------------------------------------------

import Spec.Sample.Stake (DepositWithdrawExample (DepositWithdrawExample, delta, startAmount), signer)
import Spec.Sample.Stake qualified as Stake
import Spec.Util (policyFailsWith, policySucceedsWith, toDatum, validatorFailsWith, validatorSucceedsWith)

--------------------------------------------------------------------------------

-- | Stake tests.
tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "stakeCreation"
          (stakePolicy Stake.stake)
          ()
          Stake.stakeCreation
      , policyFailsWith
          "stakeCreationWrongDatum"
          (stakePolicy Stake.stake)
          ()
          Stake.stakeCreationWrongDatum
      , policyFailsWith
          "stakeCreationUnsigned"
          (stakePolicy Stake.stake)
          ()
          Stake.stakeCreationUnsigned
      ]
  , testGroup
      "validator"
      [ validatorSucceedsWith
          "stakeDepositWithdraw deposit"
          (stakeValidator Stake.stake)
          (toDatum $ StakeDatum 100_000 signer [])
          (toDatum $ DepositWithdraw 100_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = 100_000})
      , validatorSucceedsWith
          "stakeDepositWithdraw withdraw"
          (stakeValidator Stake.stake)
          (toDatum $ StakeDatum 100_000 signer [])
          (toDatum $ DepositWithdraw $ negate 100_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 100_000})
      , validatorFailsWith
          "stakeDepositWithdraw negative GT"
          (stakeValidator Stake.stake)
          (toDatum $ StakeDatum 100_000 signer [])
          (toDatum $ DepositWithdraw 1_000_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 1_000_000})
      ]
  ]
