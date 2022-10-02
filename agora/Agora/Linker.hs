module Agora.Linker (linker) where

import Agora.Governor (Governor (gstOutRef, gtClassRef, maximumCosigners))
import Agora.Utils (validatorHashToAddress)
import Data.Tagged (untag)
import Data.Text (Text, unpack)
import Plutarch.Api.V2 (mintingPolicySymbol, validatorHash)
import PlutusLedgerApi.V1 (TxOutRef, Address, CurrencySymbol, Script)
import PlutusLedgerApi.V1.Value (AssetClass(AssetClass))
import Ply (
  TypedScriptEnvelope,
  ScriptRole(MintingPolicyRole, ValidatorRole),
  TypedScript,
  (#),
  toMintingPolicy,
  toValidator,
  toScript
 )
import Ply.Core.TypedReader (TypedReader, mkTypedScript)

import Data.Map (Map, fromList, (!?))
import Control.Arrow (left)

import Prelude hiding ((#))

linker :: Governor -> Map Text TypedScriptEnvelope -> Map Text Script
linker governor as = either error id $ do
  govPol <- get @'MintingPolicyRole @'[TxOutRef] "agora:governorPolicy"
  govVal <- get @'ValidatorRole @'[Address, CurrencySymbol, CurrencySymbol, CurrencySymbol, CurrencySymbol] "agora:governorValidator"
  stkPol <- get @'MintingPolicyRole @'[AssetClass] "agora:stakePolicy"
  stkVal <- get @'ValidatorRole @'[CurrencySymbol, AssetClass, AssetClass] "agora:stakeValidator"
  prpPol <- get @'MintingPolicyRole @'[AssetClass] "agora:proposalPolicy"
  prpVal <- get @'ValidatorRole @'[CurrencySymbol, CurrencySymbol, CurrencySymbol, Integer] "agora:proposalValidator"
  treVal <- get @'ValidatorRole @'[CurrencySymbol] "agora:treasuryValidator"
  atkPol <- get @'MintingPolicyRole @'[AssetClass] "agora:authorityTokenPolicy"
  noOpVal <- get @'ValidatorRole @'[CurrencySymbol] "agora:noOpValidator"
  treaWithdrawalVal <- get @'ValidatorRole @'[CurrencySymbol] "agora:treasuryWithdrawalValidator"
  mutateGovVal <- get @'ValidatorRole @'[AssetClass, CurrencySymbol] "agora:mutateGovernorValidator"

  let govPol' = govPol # governor.gstOutRef
      govVal' =
        govVal #
          propValAddress #
          sstSymbol #
          gstSymbol #
          pstSymbol #
          atSymbol
      gstSymbol =
        mintingPolicySymbol $
          toMintingPolicy $
            govPol'
      gstAssetClass =
        AssetClass (gstSymbol, "")

      at = gstAssetClass
      atPol' = atkPol # at
      atSymbol = mintingPolicySymbol $ toMintingPolicy $ atPol'

      propPol' = prpPol # gstAssetClass
      propVal' =
        prpVal #
          sstSymbol #
          gstSymbol #
          pstSymbol #
          governor.maximumCosigners
      propValAddress =
        validatorHashToAddress $ validatorHash $ toValidator propVal'
      pstSymbol = mintingPolicySymbol $ toMintingPolicy propPol'
      pstAssetClass = AssetClass (pstSymbol, "")

      stakPol' = stkPol # untag governor.gtClassRef
      stakVal' = stkVal # sstSymbol # pstAssetClass # untag governor.gtClassRef
      sstSymbol = mintingPolicySymbol $ toMintingPolicy stakPol'

      treaVal' = treVal # atSymbol

      noOpVal' = noOpVal # atSymbol
      treaWithdrawalVal' = treaWithdrawalVal # atSymbol
      mutateGovVal' = mutateGovVal # gstAssetClass # atSymbol

  return $
    fromList
    [ ("agora:governorPolicy", toScript govPol')
    , ("agora:governorValidator", toScript govVal')
    , ("agora:stakePolicy", toScript stakPol')
    , ("agora:stakeValidator", toScript stakVal')
    , ("agora:proposalPolicy", toScript propPol')
    , ("agora:proposalValidator", toScript propVal')
    , ("agora:treasuryValidator", toScript treaVal')
    , ("agora:authorityTokenPolicy", toScript atPol')
    , ("agora:noOpValidator", toScript noOpVal')
    , ("agora:treasuryWithdrawalValidator", toScript treaWithdrawalVal')
    , ("agora:mutateGovernorValidator", toScript mutateGovVal')
    ]
  where
    get ::
      forall (rl :: ScriptRole) (params :: [Type]).
      TypedReader rl params =>
      Text ->
      Either String (TypedScript rl params)
    get t = do
      en <- maybe (Left $ unpack t <> " not found") Right $ as !? t
      left show $ mkTypedScript en
