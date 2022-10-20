{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Agora.Linker (linker, AgoraScriptInfo (..)) where

import Agora.Governor (Governor (gstOutRef, gtClassRef, maximumCosigners))
import Agora.Utils (validatorHashToAddress, validatorHashToTokenName)
import Data.Aeson qualified as Aeson
import Data.Map (fromList)
import Data.Tagged (untag)
import Plutarch.Api.V2 (mintingPolicySymbol, validatorHash)
import PlutusLedgerApi.V1 (Address, CurrencySymbol, TxOutRef, ValidatorHash)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  toMintingPolicy,
  toScript,
  toValidator,
  (#),
 )
import ScriptExport.ScriptInfo (
  Linker,
  ScriptExport (..),
  fetchTS,
  getParam,
 )
import Prelude hiding ((#))

{- | Additional information provided after linking.

 @since 1.0.0
-}
data AgoraScriptInfo = AgoraScriptInfo
  { governorAssetClass :: AssetClass
  , authorityTokenSymbol :: CurrencySymbol
  , proposalAssetClass :: AssetClass
  , stakeAssetClass :: AssetClass
  , governor :: Governor
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

{- | Links parameterized Agora scripts given parameters.

 @since 1.0.0
-}
linker :: Linker Governor (ScriptExport AgoraScriptInfo)
linker = do
  govPol <- fetchTS @MintingPolicyRole @'[TxOutRef] "agora:governorPolicy"
  govVal <- fetchTS @ValidatorRole @'[Address, CurrencySymbol, CurrencySymbol, CurrencySymbol, CurrencySymbol] "agora:governorValidator"
  stkPol <- fetchTS @MintingPolicyRole @'[AssetClass] "agora:stakePolicy"
  stkVal <- fetchTS @ValidatorRole @'[CurrencySymbol, AssetClass, AssetClass] "agora:stakeValidator"
  prpPol <- fetchTS @MintingPolicyRole @'[AssetClass] "agora:proposalPolicy"
  prpVal <- fetchTS @ValidatorRole @'[AssetClass, CurrencySymbol, CurrencySymbol, Integer] "agora:proposalValidator"
  treVal <- fetchTS @ValidatorRole @'[CurrencySymbol] "agora:treasuryValidator"
  atkPol <- fetchTS @MintingPolicyRole @'[AssetClass] "agora:authorityTokenPolicy"
  noOpVal <- fetchTS @ValidatorRole @'[CurrencySymbol] "agora:noOpValidator"
  treaWithdrawalVal <- fetchTS @ValidatorRole @'[CurrencySymbol] "agora:treasuryWithdrawalValidator"
  mutateGovVal <- fetchTS @ValidatorRole @'[ValidatorHash, CurrencySymbol, CurrencySymbol] "agora:mutateGovernorValidator"

  governor <- getParam

  let govPol' = govPol # governor.gstOutRef
      govVal' =
        govVal
          # propValAddress
          # sstSymbol
          # gstSymbol
          # pstSymbol
          # atSymbol
      gstSymbol =
        mintingPolicySymbol $
          toMintingPolicy
            govPol'
      gstAssetClass =
        AssetClass (gstSymbol, "")
      govValHash = validatorHash $ toValidator govVal'

      at = gstAssetClass
      atPol' = atkPol # at
      atSymbol = mintingPolicySymbol $ toMintingPolicy atPol'

      propPol' = prpPol # gstAssetClass
      propVal' =
        prpVal
          # sstAssetClass
          # gstSymbol
          # pstSymbol
          # governor.maximumCosigners
      propValAddress =
        validatorHashToAddress $ validatorHash $ toValidator propVal'
      pstSymbol = mintingPolicySymbol $ toMintingPolicy propPol'
      pstAssetClass = AssetClass (pstSymbol, "")

      stakPol' = stkPol # untag governor.gtClassRef
      stakVal' = stkVal # sstSymbol # pstAssetClass # untag governor.gtClassRef
      sstSymbol = mintingPolicySymbol $ toMintingPolicy stakPol'
      stakValTokenName =
        validatorHashToTokenName $ validatorHash $ toValidator stakVal'
      sstAssetClass = AssetClass (sstSymbol, stakValTokenName)

      treaVal' = treVal # atSymbol

      noOpVal' = noOpVal # atSymbol
      treaWithdrawalVal' = treaWithdrawalVal # atSymbol
      mutateGovVal' = mutateGovVal # govValHash # gstSymbol # atSymbol

  return $
    ScriptExport
      { scripts =
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
      , information =
          AgoraScriptInfo
            { governorAssetClass = gstAssetClass
            , authorityTokenSymbol = atSymbol
            , proposalAssetClass = pstAssetClass
            , stakeAssetClass = sstAssetClass
            , governor = governor
            }
      }
