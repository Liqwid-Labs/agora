module Spec.Stake (tests) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

import Plutarch.Builtin (pforgetData)

--------------------------------------------------------------------------------

import Agora.Stake (StakeDatum (StakeDatum), StakeRedeemer (DepositWithdraw), stakePolicy, stakeValidator)

--------------------------------------------------------------------------------

import Spec.Sample.Stake (DepositWithdrawExample (DepositWithdrawExample, delta, startAmount), signer)
import Spec.Sample.Stake qualified as Stake
import Spec.Util (policyFailsWith, policySucceedsWith, toDatum, validatorFailsWith, validatorSucceedsWith)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "stakeCreation"
          (stakePolicy Stake.stake)
          (pforgetData (pconstantData ()))
          Stake.stakeCreation
      , policyFailsWith
          "stakeCreationWrongDatum"
          (stakePolicy Stake.stake)
          (pforgetData (pconstantData ()))
          Stake.stakeCreationWrongDatum
      , policyFailsWith
          "stakeCreationUnsigned"
          (stakePolicy Stake.stake)
          (pforgetData (pconstantData ()))
          Stake.stakeCreationUnsigned
      , validatorSucceedsWith
          "stakeDepositWithdraw deposit"
          (stakeValidator Stake.stake)
          (pforgetData (pconstantData . toDatum $ StakeDatum 100_000 signer))
          (pforgetData (pconstantData . toDatum $ DepositWithdraw 100_000))
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = 100_000})
      , validatorSucceedsWith
          "stakeDepositWithdraw withdraw"
          (stakeValidator Stake.stake)
          (pforgetData (pconstantData . toDatum $ StakeDatum 100_000 signer))
          (pforgetData (pconstantData . toDatum $ DepositWithdraw (negate 100_000)))
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 100_000})
      , validatorFailsWith
          "stakeDepositWithdraw negative GT"
          (stakeValidator Stake.stake)
          (pforgetData (pconstantData . toDatum $ StakeDatum 100_000 signer))
          (pforgetData (pconstantData . toDatum $ DepositWithdraw (negate 1_000_000)))
          (Stake.stakeDepositWithdraw $ DepositWithdrawExample {startAmount = 100_000, delta = negate 1_000_000})
      ]
  ]
