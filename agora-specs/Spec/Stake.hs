{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Stake
Maintainer : emi@haskell.fyi
Description: Tests for Stake policy and validator

Tests for Stake policy and validator
-}
module Spec.Stake (specs) where

import Agora.Stake (
  StakeDatum (StakeDatum),
  StakeRedeemer (DepositWithdraw),
 )
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import PlutusLedgerApi.V1 (Credential (PubKeyCredential))
import Sample.Shared (stakePolicy, stakeValidator)
import Sample.Stake (
  DepositWithdrawExample (
    DepositWithdrawExample,
    delta,
    startAmount
  ),
  signer,
 )
import Sample.Stake qualified as Stake (
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
import Prelude (Num (negate), ($))

-- | The SpecificationTree exported by this module.
specs :: [SpecificationTree]
specs =
  [ group
      "policy"
      [ policySucceedsWith
          "stakeCreation"
          stakePolicy
          ()
          Stake.stakeCreation
      , policyFailsWith
          "stakeCreationWrongDatum"
          stakePolicy
          ()
          Stake.stakeCreationWrongDatum
      , policyFailsWith
          "stakeCreationUnsigned"
          stakePolicy
          ()
          Stake.stakeCreationUnsigned
      ]
  , group
      "validator"
      [ validatorSucceedsWith
          "stakeDepositWithdraw deposit"
          stakeValidator
          (StakeDatum 100_000 (PubKeyCredential signer) Nothing [])
          (DepositWithdraw 100_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = 100_000})
      , validatorSucceedsWith
          "stakeDepositWithdraw withdraw"
          stakeValidator
          (StakeDatum 100_000 (PubKeyCredential signer) Nothing [])
          (DepositWithdraw $ negate 100_000)
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 100_000})
      , validatorFailsWith
          "stakeDepositWithdraw negative GT"
          stakeValidator
          (StakeDatum 100_000 (PubKeyCredential signer) Nothing [])
          (DepositWithdraw 1_000_000)
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
