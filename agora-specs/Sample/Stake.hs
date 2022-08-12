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
  stakeCreation,
  stakeCreationWrongDatum,
  stakeCreationUnsigned,
  stakeDepositWithdraw,
  DepositWithdrawExample (..),
) where

import Agora.Governor (Governor (gtClassRef))
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  StakeDatum (StakeDatum, stakedAmount),
 )
import Data.Tagged (Tagged, untag)
import Plutarch.Context (
  MintingBuilder,
  SpendingBuilder,
  buildMintingUnsafe,
  buildSpendingUnsafe,
  input,
  mint,
  output,
  script,
  signedWith,
  txId,
  withDatum,
  withMinting,
  withOutRef,
  withSpendingOutRef,
  withValue,
 )
import PlutusLedgerApi.V1 (
  Datum (Datum),
  ScriptContext (..),
  ScriptPurpose (Minting),
  ToData (toBuiltinData),
  TxInfo (txInfoData, txInfoSignatories),
 )
import PlutusLedgerApi.V1.Contexts (TxOutRef (..))
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClassValue,
  singleton,
 )
import Sample.Shared (
  governor,
  signer,
  stakeAssetClass,
  stakeSymbol,
  stakeValidatorHash,
 )
import Test.Util (sortValue)

-- | This script context should be a valid transaction.
stakeCreation :: ScriptContext
stakeCreation =
  let st = Value.assetClassValue stakeAssetClass 1 -- Stake ST
      datum :: StakeDatum
      datum = StakeDatum 424242424242 signer Nothing []

      builder :: MintingBuilder
      builder =
        mconcat
          [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          , signedWith signer
          , mint st
          , output $
              mconcat
                [ script stakeValidatorHash
                , withValue (st <> Value.singleton "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24" "LQ" 424242424242)
                , withDatum datum
                ]
          , withMinting stakeSymbol
          ]
   in buildMintingUnsafe builder

-- | This ScriptContext should fail because the datum has too much GT.
stakeCreationWrongDatum :: ScriptContext
stakeCreationWrongDatum =
  let datum :: Datum
      datum = Datum (toBuiltinData $ StakeDatum 4242424242424242 signer Nothing []) -- Too much GT
   in ScriptContext
        { scriptContextTxInfo = stakeCreation.scriptContextTxInfo {txInfoData = [("", datum)]}
        , scriptContextPurpose = Minting stakeSymbol
        }

-- | This ScriptContext should fail because the datum has too much GT.
stakeCreationUnsigned :: ScriptContext
stakeCreationUnsigned =
  ScriptContext
    { scriptContextTxInfo =
        stakeCreation.scriptContextTxInfo
          { txInfoSignatories = []
          }
    , scriptContextPurpose = Minting stakeSymbol
    }

--------------------------------------------------------------------------------

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
  let st = Value.assetClassValue stakeAssetClass 1 -- Stake ST
      stakeBefore :: StakeDatum
      stakeBefore = StakeDatum config.startAmount signer Nothing []

      stakeAfter :: StakeDatum
      stakeAfter = stakeBefore {stakedAmount = stakeBefore.stakedAmount + config.delta}

      stakeRef :: TxOutRef
      stakeRef = TxOutRef "0ffef57e30cc604342c738e31e0451593837b313e7bfb94b0922b142782f98e6" 1

      builder :: SpendingBuilder
      builder =
        mconcat
          [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          , signedWith signer
          , mint st
          , input $
              mconcat
                [ script stakeValidatorHash
                , withValue
                    ( sortValue $
                        st
                          <> Value.assetClassValue (untag governor.gtClassRef) (untag stakeBefore.stakedAmount)
                    )
                , withDatum stakeAfter
                , withOutRef stakeRef
                ]
          , output $
              mconcat
                [ script stakeValidatorHash
                , withValue
                    ( sortValue $
                        st
                          <> Value.assetClassValue (untag governor.gtClassRef) (untag stakeAfter.stakedAmount)
                    )
                , withDatum stakeAfter
                ]
          , withSpendingOutRef stakeRef
          ]
   in buildSpendingUnsafe builder
