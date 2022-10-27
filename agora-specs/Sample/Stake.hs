{- |
Module     : Sample.Stake
Maintainer : emi@haskell.fyi
Description: Sample based testing for Stake utxos

This module tests primarily the happy path for Stake creation
-}
module Sample.Stake (
  stakeAssetClass,
  stakeSymbol,
  signer,

  -- * Script contexts
  stakeDepositWithdraw,
  DepositWithdrawExample (..),
) where

import Agora.Governor (Governor (gtClassRef))
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  StakeDatum (StakeDatum, stakedAmount),
 )
import Data.Tagged (Tagged)
import Plutarch.Context (
  SpendingBuilder,
  buildSpending',
  input,
  output,
  script,
  signedWith,
  txId,
  withDatum,
  withRef,
  withSpendingOutRef,
  withValue,
 )
import Plutarch.Extra.AssetClass (assetClassValue)
import PlutusLedgerApi.V1.Contexts (TxOutRef (..))
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  ScriptContext (..),
 )
import Sample.Shared (
  governor,
  signer,
  stakeAssetClass,
  stakeSymbol,
  stakeValidatorHash,
 )
import Test.Util (sortValue)

-- | Config for creating a ScriptContext that deposits or withdraws.
data DepositWithdrawExample = DepositWithdrawExample
  { startAmount :: Tagged GTTag Integer
  -- ^ The amount of GT stored before the transaction.
  , delta :: Tagged GTTag Integer
  -- ^ The amount of GT deposited or withdrawn from the Stake.
  }

-- | Create a ScriptContext that deposits or withdraws, given the config for it.
stakeDepositWithdraw :: DepositWithdrawExample -> ScriptContext
stakeDepositWithdraw config =
  let st = assetClassValue stakeAssetClass 1 -- Stake ST
      stakeBefore :: StakeDatum
      stakeBefore = StakeDatum config.startAmount (PubKeyCredential signer) Nothing []

      stakeAfter :: StakeDatum
      stakeAfter = stakeBefore {stakedAmount = stakeBefore.stakedAmount + config.delta}

      stakeRef :: TxOutRef
      stakeRef = TxOutRef "0ffef57e30cc604342c738e31e0451593837b313e7bfb94b0922b142782f98e6" 1

      builder :: SpendingBuilder
      builder =
        mconcat
          [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          , signedWith signer
          , input $
              mconcat
                [ script stakeValidatorHash
                , withValue
                    ( sortValue $
                        st
                          <> assetClassValue governor.gtClassRef stakeBefore.stakedAmount
                    )
                , withDatum stakeBefore
                , withRef stakeRef
                ]
          , output $
              mconcat
                [ script stakeValidatorHash
                , withValue
                    ( sortValue $
                        st
                          <> assetClassValue governor.gtClassRef stakeAfter.stakedAmount
                    )
                , withDatum stakeAfter
                ]
          , withSpendingOutRef stakeRef
          ]
   in buildSpending' builder
