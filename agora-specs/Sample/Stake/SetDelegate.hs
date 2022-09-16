{- |
Module     : Sample.Stake.SetDelegate
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of setting the delegate.

Sample and utilities for testing the functionalities of setting the delegate.
-}
module Sample.Stake.SetDelegate (
  Parameters (..),
  setDelegate,
  mkStakeRedeemer,
  mkStakeInputDatum,
  mkTestCase,
  overrideExistingDelegateParameters,
  clearDelegateParameters,
  setDelegateParameters,
  invalidOutputStakeDatumParameters,
  ownerDoesntSignParameters,
  delegateToOwnerParameters,
) where

import Agora.Governor (Governor (gtClassRef))
import Agora.Scripts (AgoraScripts (..))
import Agora.Stake (
  StakeDatum (..),
  StakeRedeemer (ClearDelegate, DelegateTo),
 )
import Data.Tagged (untag)
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
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  PubKeyHash,
  ScriptContext,
  TxOutRef (TxOutRef),
 )
import Sample.Shared (
  agoraScripts,
  fromDiscrete,
  governor,
  minAda,
  signer,
  signer2,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Specification (SpecificationTree, testValidator)
import Test.Util (pubKeyHashes, sortValue)

--------------------------------------------------------------------------------

-- | Parameters that control the script context generation of 'setDelegate'.
data Parameters = Parameters
  { existingDelegate :: Maybe PubKeyHash
  -- ^ Whom the stake has been delegated to.
  , newDelegate :: Maybe PubKeyHash
  -- ^ The new delegate to set to.
  , invalidOutputStake :: Bool
  -- ^ The output stake datum will be invalid if this is set to true.
  , signedByOwner :: Bool
  -- ^ Whether the stake owner signs the transaction o not.
  }

-- | Select the correct stake redeemer based on the existence of the new delegate.
mkStakeRedeemer :: Parameters -> StakeRedeemer
mkStakeRedeemer params = maybe ClearDelegate (DelegateTo . PubKeyCredential) params.newDelegate

-- | The owner of the input stake.
stakeOwner :: PubKeyHash
stakeOwner = signer

-- | Create input stake datum given the parameters.
mkStakeInputDatum :: Parameters -> StakeDatum
mkStakeInputDatum ps =
  StakeDatum
    { stakedAmount = 5
    , owner = PubKeyCredential stakeOwner
    , delegatedTo = PubKeyCredential <$> ps.existingDelegate
    , lockedBy = []
    }

-- | Generate a 'ScriptContext' that tries to change the delegate of a stake.
setDelegate :: Parameters -> ScriptContext
setDelegate ps = buildSpending' builder
  where
    stakeRef :: TxOutRef
    stakeRef = TxOutRef "0ffef57e30cc604342c738e31e0451593837b313e7bfb94b0922b142782f98e6" 1

    stakeInput = mkStakeInputDatum ps

    stakeOutput =
      let stakedAmount =
            if ps.invalidOutputStake
              then stakeInput.stakedAmount - 1
              else stakeInput.stakedAmount
       in stakeInput
            { stakedAmount = stakedAmount
            , delegatedTo = PubKeyCredential <$> ps.newDelegate
            }

    signer =
      if ps.signedByOwner
        then case stakeInput.owner of
          PubKeyCredential c -> c
          _ -> signer2
        else signer2

    st = Value.assetClassValue stakeAssetClass 1 -- Stake ST
    stakeValue =
      sortValue $
        mconcat
          [ st
          , Value.assetClassValue
              (untag governor.gtClassRef)
              (fromDiscrete stakeInput.stakedAmount)
          , minAda
          ]

    builder :: SpendingBuilder
    builder =
      mconcat
        [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
        , signedWith signer
        , input $
            mconcat
              [ script stakeValidatorHash
              , withValue stakeValue
              , withDatum stakeInput
              , withRef stakeRef
              ]
        , output $
            mconcat
              [ script stakeValidatorHash
              , withValue stakeValue
              , withDatum stakeOutput
              ]
        , withSpendingOutRef stakeRef
        ]

--------------------------------------------------------------------------------

{- | Create a test case that runs the stake validator to test the functionality
      of setting the delegate.P
-}
mkTestCase :: String -> Parameters -> Bool -> SpecificationTree
mkTestCase name ps valid =
  testValidator
    valid
    name
    agoraScripts.compiledStakeValidator
    (mkStakeInputDatum ps)
    (mkStakeRedeemer ps)
    (setDelegate ps)

--------------------------------------------------------------------------------

-- * Valid Parameters

overrideExistingDelegateParameters :: Parameters
overrideExistingDelegateParameters =
  Parameters
    { existingDelegate = Just $ head pubKeyHashes
    , newDelegate = Just $ pubKeyHashes !! 2
    , invalidOutputStake = False
    , signedByOwner = True
    }

clearDelegateParameters :: Parameters
clearDelegateParameters =
  overrideExistingDelegateParameters
    { newDelegate = Nothing
    }

setDelegateParameters :: Parameters
setDelegateParameters =
  overrideExistingDelegateParameters
    { existingDelegate = Nothing
    }

--------------------------------------------------------------------------------

-- * Invalid Parameters

ownerDoesntSignParameters :: Parameters
ownerDoesntSignParameters =
  overrideExistingDelegateParameters
    { signedByOwner = False
    }

delegateToOwnerParameters :: Parameters
delegateToOwnerParameters =
  overrideExistingDelegateParameters
    { existingDelegate = Nothing
    , newDelegate = Just stakeOwner
    }

invalidOutputStakeDatumParameters :: Parameters
invalidOutputStakeDatumParameters =
  overrideExistingDelegateParameters
    { invalidOutputStake = True
    }
