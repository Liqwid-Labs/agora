{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Stake
Maintainer : emi@haskell.fyi
Description: Tests for Stake policy and validator

Tests for Stake policy and validator
-}
module Spec.Stake (specs) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Agora.Stake (Stake (..), StakeDatum (StakeDatum), StakeRedeemer (DepositWithdraw))
import Agora.Stake.Scripts (stakePolicy, stakeValidator)

--------------------------------------------------------------------------------

import Sample.Stake (DepositWithdrawExample (DepositWithdrawExample, delta, startAmount), signer)
import Sample.Stake qualified as Stake
import Test.Specification (
  SpecificationTree,
  group,
  policyFailsWith,
  policySucceedsWith,
  validatorFailsWith,
  validatorSucceedsWith,
 )
import Test.Util (toDatum)

--------------------------------------------------------------------------------

-- | The SpecificationTree exported by this module.
specs :: [SpecificationTree]
specs =
  [ group
      "policy"
      [ policySucceedsWith
          "stakeCreation"
          (stakePolicy Stake.stake.gtClassRef)
          ()
          Stake.stakeCreation
      , policyFailsWith
          "stakeCreationWrongDatum"
          (stakePolicy Stake.stake.gtClassRef)
          ()
          Stake.stakeCreationWrongDatum
      , policyFailsWith
          "stakeCreationUnsigned"
          (stakePolicy Stake.stake.gtClassRef)
          ()
          Stake.stakeCreationUnsigned
      ]
  , group
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
