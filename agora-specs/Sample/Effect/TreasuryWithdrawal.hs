{- |
Module     : Sample.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module provides samples for Treasury Withdrawal Effect tests.
-}
module Sample.Effect.TreasuryWithdrawal (
  runEffect,
  Parameters (..),
  Validity (..),
  totallyValidParameters,
  mkTestTree,
) where

import Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (..),
 )
import Control.Composition ((.*))
import Data.Foldable (Foldable (fold))
import Data.List (singleton)
import Data.Map ((!))
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as Map
import Data.Semigroup (mtimesDefault)
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Context (credential, input, mint, output, script, withInlineDatum, withRef, withRefTxId, withValue)
import Plutarch.Script (Script)
import PlutusLedgerApi.V1.Value qualified as Value (scale, singleton)
import PlutusLedgerApi.V2 (
  Credential (..),
  TxId,
  TxOutRef (TxOutRef),
  Value,
 )
import PlutusLedgerApi.V3 (ScriptHash)
import Sample.Shared (agoraScripts, authorityTokenPolicy, authorityTokenSymbol, signer, signer2, trScriptHash, trValidator)
import Test.Specification (SpecificationTree, group, testPolicy, testValidator)
import Test.Util (CombinableBuilder, mkMinting, mkSpending, subtractValue, validatorHashes)

data Parameters = Parameters
  { shouldDeliver ::
      OMap Credential Value
  , treasuryInputCount :: Integer
  , badReceivedValue :: Bool
  , badReceivers :: Bool
  , badReceiverOrder :: Bool
  , badTreasuryPaybackValue :: Bool
  }

data Validity = Validity
  { forGATPolicy :: Bool
  , forEffectValidator :: Bool
  , forTreasury :: Bool
  }

effectValidator :: Script
effectValidator = agoraScripts ! "agora:treasuryWithdrawalValidator"

effectHash :: ScriptHash
effectHash = scriptHash effectValidator

mkEffectDatum :: Parameters -> TreasuryWithdrawalDatum
mkEffectDatum ps =
  TreasuryWithdrawalDatum
    { receivers = Map.assocs ps.shouldDeliver
    , treasuries = [ScriptCredential trScriptHash]
    }

effectRef :: TxOutRef
effectRef = TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0

treasuryTxId :: TxId
treasuryTxId = "0ca36f3a357bc69579ab2531aecd1e7d3714d993c7820f40b864be15"

mkEffectInputBuilder :: forall b. CombinableBuilder b => Parameters -> b
mkEffectInputBuilder ps =
  let mkGATValue = Value.singleton authorityTokenSymbol ""
   in mconcat
        [ mint $ mkGATValue (-1)
        , input $
            mconcat
              [ script effectHash
              , withRef effectRef
              , withInlineDatum $ mkEffectDatum ps
              , withValue $ mkGATValue 1
              ]
        ]

mkTreasuryInputBuilder ::
  forall b.
  CombinableBuilder b =>
  Parameters ->
  b
mkTreasuryInputBuilder ps =
  mtimesDefault ps.treasuryInputCount $
    input $
      mconcat
        [ script trScriptHash
        , withRefTxId treasuryTxId
        , withInlineDatum ()
        , withValue $ fold ps.shouldDeliver
        ]

mkTreasuryPaybackOutputBuilder ::
  forall b.
  CombinableBuilder b =>
  Parameters ->
  b
mkTreasuryPaybackOutputBuilder ps =
  let sentAmount = fold ps.shouldDeliver
      inputAmount =
        flip Value.scale sentAmount $
          if ps.badTreasuryPaybackValue
            then 1
            else ps.treasuryInputCount
      paybackValue = inputAmount `subtractValue` sentAmount
   in output $
        mconcat
          [ script trScriptHash
          , withValue paybackValue
          , withInlineDatum ()
          ]

mkReceiverOutputBuilder ::
  forall b.
  CombinableBuilder b =>
  Parameters ->
  b
mkReceiverOutputBuilder ps =
  let mkOutputValue =
        if ps.badReceivedValue
          then const $ Value.singleton "" "bruh" 1
          else id
      mkFinalOutputs =
        mconcat
          . (if ps.badReceiverOrder then reverse else id)
          . (if ps.badReceivers then drop 1 else id)
      mkOutput :: _ -> _ -> b
      mkOutput cred value =
        output $
          mconcat
            [ credential cred
            , withValue $ mkOutputValue value
            , withInlineDatum ()
            ]
      rawOutputs =
        foldMap (uncurry $ singleton .* mkOutput) $
          Map.assocs ps.shouldDeliver
   in mkFinalOutputs rawOutputs

runEffect :: forall b. CombinableBuilder b => Parameters -> b
runEffect ps =
  foldMap
    ($ ps)
    [ mkEffectInputBuilder
    , mkTreasuryInputBuilder
    , mkReceiverOutputBuilder
    , mkTreasuryPaybackOutputBuilder
    ]

totallyValidParameters :: Parameters
totallyValidParameters =
  Parameters
    { shouldDeliver =
        Map.fromList
          [ (PubKeyCredential signer, Value.singleton "" "" 42_000_000)
          , (PubKeyCredential signer2, Value.singleton "" "" 42_000_000)
          , (ScriptCredential (head validatorHashes), Value.singleton "" "" 42_000_000)
          ]
    , treasuryInputCount = 2
    , badReceivedValue = False
    , badReceivers = False
    , badReceiverOrder = False
    , badTreasuryPaybackValue = False
    }

mkTestTree ::
  String ->
  Parameters ->
  Validity ->
  SpecificationTree
mkTestTree name ps val =
  group name [effect, treasury, authority]
  where
    spend = mkSpending runEffect ps
    mint = mkMinting runEffect ps

    effect =
      testValidator
        val.forEffectValidator
        "effect"
        effectValidator
        (mkEffectDatum ps)
        ()
        (spend effectRef)

    treasury =
      testValidator
        val.forTreasury
        "treasury"
        trValidator
        ()
        ()
        (spend $ TxOutRef treasuryTxId 1)

    authority =
      testPolicy
        val.forGATPolicy
        "authority"
        authorityTokenPolicy
        ()
        (mint authorityTokenSymbol)
