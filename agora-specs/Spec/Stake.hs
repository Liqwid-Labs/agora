{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Stake
Maintainer : emi@haskell.fyi
Description: Tests for Stake policy and validator

Tests for Stake policy and validator
-}
module Spec.Stake (specs) where

import Agora.Stake (
  Stake (..),
  StakeDatum (StakeDatum),
  StakeRedeemer (DepositWithdraw),
 )
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Sample.Stake (
  DepositWithdrawExample (
    DepositWithdrawExample,
    delta,
    startAmount
  ),
  signer,
 )
import Sample.Stake qualified as Stake (
  stake,
  stakeCreation,
  stakeCreationUnsigned,
  stakeCreationWrongDatum,
  stakeDepositWithdraw,
 )
import Sample.Stake.SetDelegate qualified as SetDelegate
import Test.Specification (
  SpecificationTree,
  group,
  policyFailsWith,
  policySucceedsWith,
  validatorFailsWith,
  validatorSucceedsWith,
 )
import Test.Util (toDatum)
import Prelude (Num (negate), ($))

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
          (toDatum $ StakeDatum 100_000 signer Nothing [])
          (toDatum $ DepositWithdraw 100_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = 100_000})
      , validatorSucceedsWith
          "stakeDepositWithdraw withdraw"
          (stakeValidator Stake.stake)
          (toDatum $ StakeDatum 100_000 signer Nothing [])
          (toDatum $ DepositWithdraw $ negate 100_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 100_000})
      , validatorFailsWith
          "stakeDepositWithdraw negative GT"
          (stakeValidator Stake.stake)
          (toDatum $ StakeDatum 100_000 signer Nothing [])
          (toDatum $ DepositWithdraw 1_000_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 1_000_000})
      , group
          "set delegate"
          [ SetDelegate.mkTestCase
              "override existing delegate"
              SetDelegate.overrideExistingDelegateParameters
              True
          , SetDelegate.mkTestCase
              "remove existing delegate"
              SetDelegate.clearDelegateParameters
              True
          , SetDelegate.mkTestCase
              "set delegate to something"
              SetDelegate.setDelegateParameters
              True
          , SetDelegate.mkTestCase
              "owner doesn't sign the transaction"
              SetDelegate.ownerDoesntSignParameters
              False
          , SetDelegate.mkTestCase
              "delegate to the owner"
              SetDelegate.delegateToOwnerParameters
              False
          , SetDelegate.mkTestCase
              "invalid output stake"
              SetDelegate.invalidOutputStakeDatumParameters
              False
          ]
      ]
  ]
