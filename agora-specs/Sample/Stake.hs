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
import Data.Tagged (untag)
import Plutarch.Context (
  MintingBuilder,
  SpendingBuilder,
  buildMinting',
  buildSpending',
  input,
  mint,
  output,
  script,
  signedWith,
  txId,
  withDatum,
  withMinting,
  withRef,
  withSpendingOutRef,
  withValue,
 )
import Plutarch.SafeMoney (Discrete)
import PlutusLedgerApi.V1.Contexts (TxOutRef (..))
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClassValue,
  singleton,
 )
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  Datum (Datum),
  ScriptContext (..),
  ScriptPurpose (Minting),
  ToData (toBuiltinData),
  TxInfo (txInfoData, txInfoSignatories),
 )
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Shared (
  fromDiscrete,
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
      datum = StakeDatum 424242424242 (PubKeyCredential signer) Nothing []

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
   in buildMinting' builder

-- | This ScriptContext should fail because the datum has too much GT.
stakeCreationWrongDatum :: ScriptContext
stakeCreationWrongDatum =
  let datum :: Datum
      datum = Datum (toBuiltinData $ StakeDatum 4242424242424242 (PubKeyCredential signer) Nothing []) -- Too much GT
   in ScriptContext
        { scriptContextTxInfo = stakeCreation.scriptContextTxInfo {txInfoData = AssocMap.fromList [("", datum)]}
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
  { startAmount :: Discrete GTTag
  -- ^ The amount of GT stored before the transaction.
  , delta :: Discrete GTTag
  -- ^ The amount of GT deposited or withdrawn from the Stake.
  }

-- | Create a ScriptContext that deposits or withdraws, given the config for it.
stakeDepositWithdraw :: DepositWithdrawExample -> ScriptContext
stakeDepositWithdraw config =
  let st = Value.assetClassValue stakeAssetClass 1 -- Stake ST
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
                          <> Value.assetClassValue (untag governor.gtClassRef) (fromDiscrete stakeBefore.stakedAmount)
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
                          <> Value.assetClassValue (untag governor.gtClassRef) (fromDiscrete stakeAfter.stakedAmount)
                    )
                , withDatum stakeAfter
                ]
          , withSpendingOutRef stakeRef
          ]
   in buildSpending' builder
