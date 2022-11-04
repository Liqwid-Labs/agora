{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Agora.Linker (linker, AgoraScriptInfo (..)) where

import Agora.Governor (Governor (gstOutRef, gtClassRef, maximumCosigners))
import Agora.SafeMoney (AuthorityTokenTag, GTTag, GovernorSTTag, ProposalSTTag, StakeSTTag)
import Agora.Utils (validatorHashToAddress)
import Data.Aeson qualified as Aeson
import Data.Map (fromList)
import Data.Tagged (Tagged (Tagged))
import Plutarch.Api.V2 (mintingPolicySymbol, validatorHash)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.ScriptContext (validatorHashToTokenName)
import PlutusLedgerApi.V1 (Address, CurrencySymbol, TxOutRef, ValidatorHash)
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
  { governorAssetClass :: Tagged GovernorSTTag AssetClass
  , authorityTokenSymbol :: Tagged AuthorityTokenTag CurrencySymbol
  , proposalAssetClass :: Tagged ProposalSTTag AssetClass
  , stakeAssetClass :: Tagged StakeSTTag AssetClass
  , governor :: Governor
  }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

{- | Links parameterized Agora scripts given parameters.

 @since 1.0.0
-}
linker :: Linker Governor (ScriptExport AgoraScriptInfo)
linker = do
  govPol <-
    fetchTS
      @MintingPolicyRole
      @'[TxOutRef]
      "agora:governorPolicy"
  govVal <-
    fetchTS
      @ValidatorRole
      @'[ Address
        , Tagged StakeSTTag AssetClass
        , Tagged GovernorSTTag CurrencySymbol
        , Tagged ProposalSTTag CurrencySymbol
        , Tagged AuthorityTokenTag CurrencySymbol
        ]
      "agora:governorValidator"
  stkPol <-
    fetchTS
      @MintingPolicyRole
      @'[Tagged GTTag AssetClass]
      "agora:stakePolicy"
  stkVal <-
    fetchTS
      @ValidatorRole
      @'[ Tagged StakeSTTag CurrencySymbol
        , Tagged ProposalSTTag AssetClass
        , Tagged GTTag AssetClass
        ]
      "agora:stakeValidator"
  prpPol <-
    fetchTS @MintingPolicyRole
      @'[Tagged GovernorSTTag AssetClass]
      "agora:proposalPolicy"
  prpVal <-
    fetchTS
      @ValidatorRole
      @'[ Tagged StakeSTTag AssetClass
        , Tagged GovernorSTTag CurrencySymbol
        , Tagged ProposalSTTag CurrencySymbol
        , Integer
        ]
      "agora:proposalValidator"
  treVal <-
    fetchTS
      @ValidatorRole
      @'[Tagged AuthorityTokenTag CurrencySymbol]
      "agora:treasuryValidator"
  atkPol <-
    fetchTS
      @MintingPolicyRole
      @'[Tagged GovernorSTTag AssetClass]
      "agora:authorityTokenPolicy"
  noOpVal <-
    fetchTS
      @ValidatorRole
      @'[Tagged AuthorityTokenTag CurrencySymbol]
      "agora:noOpValidator"
  treaWithdrawalVal <-
    fetchTS
      @ValidatorRole
      @'[Tagged AuthorityTokenTag CurrencySymbol]
      "agora:treasuryWithdrawalValidator"
  mutateGovVal <-
    fetchTS
      @ValidatorRole
      @'[ValidatorHash, Tagged GovernorSTTag CurrencySymbol, Tagged AuthorityTokenTag CurrencySymbol]
      "agora:mutateGovernorValidator"

  governor <- getParam

  let govPol' = govPol # governor.gstOutRef
      govVal' =
        govVal
          # propValAddress
          # Tagged sstAssetClass
          # Tagged gstSymbol
          # Tagged pstSymbol
          # Tagged atSymbol
      gstSymbol =
        mintingPolicySymbol $
          toMintingPolicy
            govPol'
      gstAssetClass =
        AssetClass gstSymbol ""
      govValHash = validatorHash $ toValidator govVal'

      atPol' = atkPol # Tagged gstAssetClass
      atSymbol = mintingPolicySymbol $ toMintingPolicy atPol'

      propPol' = prpPol # Tagged gstAssetClass
      propVal' =
        prpVal
          # Tagged sstAssetClass
          # Tagged gstSymbol
          # Tagged pstSymbol
          # governor.maximumCosigners
      propValAddress =
        validatorHashToAddress $ validatorHash $ toValidator propVal'
      pstSymbol = mintingPolicySymbol $ toMintingPolicy propPol'
      pstAssetClass = AssetClass pstSymbol ""

      stakPol' = stkPol # governor.gtClassRef
      stakVal' =
        stkVal
          # Tagged sstSymbol
          # Tagged pstAssetClass
          # governor.gtClassRef
      sstSymbol = mintingPolicySymbol $ toMintingPolicy stakPol'
      stakValTokenName =
        validatorHashToTokenName $ validatorHash $ toValidator stakVal'
      sstAssetClass = AssetClass sstSymbol stakValTokenName

      treaVal' = treVal # Tagged atSymbol

      noOpVal' = noOpVal # Tagged atSymbol
      treaWithdrawalVal' = treaWithdrawalVal # Tagged atSymbol
      mutateGovVal' =
        mutateGovVal
          # govValHash
          # Tagged gstSymbol
          # Tagged atSymbol

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
            { governorAssetClass = Tagged gstAssetClass
            , authorityTokenSymbol = Tagged atSymbol
            , proposalAssetClass = Tagged pstAssetClass
            , stakeAssetClass = Tagged sstAssetClass
            , governor = governor
            }
      }
