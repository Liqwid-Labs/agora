{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Agora.Linker (linker, AgoraScriptInfo (..)) where

import Agora.Governor (Governor (gstOutRef, gtClassRef, maximumCosigners))
import Agora.SafeMoney (AuthorityTokenTag, GTTag, GovernorSTTag, ProposalSTTag, StakeSTTag)
import Data.Aeson qualified as Aeson
import Data.Map (fromList)
import Data.Tagged (Tagged (Tagged))
import Plutarch.Api.V1 (scriptHash)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import PlutusLedgerApi.V1 (CurrencySymbol (CurrencySymbol), ScriptHash, TxOutRef, getScriptHash)
import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  (#),
 )
import ScriptExport.ScriptInfo (
  Linker,
  ScriptExport (..),
  fetchTS,
  getParam,
  toRoledScript,
  toScript,
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
      @'[ ScriptHash
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
      @'[ ScriptHash
        , Tagged GovernorSTTag CurrencySymbol
        , Tagged AuthorityTokenTag CurrencySymbol
        ]
      "agora:mutateGovernorValidator"
  alwaysSucceedsPolicy' <-
    fetchTS
      @MintingPolicyRole
      @'[]
      "agora:alwaysSucceedsPolicy"

  governor <- getParam

  let govPol' = govPol # governor.gstOutRef
      govVal' =
        govVal
          # propValHash
          # Tagged sstAssetClass
          # Tagged gstSymbol
          # Tagged pstSymbol
          # Tagged atSymbol
      gstSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript govPol'
      gstAssetClass =
        AssetClass gstSymbol ""
      govValHash = scriptHash $ toScript govVal'

      atPol' = atkPol # Tagged gstAssetClass
      atSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript atPol'

      propPol' = prpPol # Tagged gstAssetClass
      propVal' =
        prpVal
          # Tagged sstAssetClass
          # Tagged gstSymbol
          # Tagged pstSymbol
          # governor.maximumCosigners
      propValHash = scriptHash $ toScript propVal'
      pstSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript propPol'
      pstAssetClass = AssetClass pstSymbol ""

      stakPol' = stkPol # governor.gtClassRef
      stakVal' =
        stkVal
          # Tagged sstSymbol
          # Tagged pstAssetClass
          # governor.gtClassRef
      sstSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript stakPol'
      stakValTokenName =
        scriptHashToTokenName $ scriptHash $ toScript stakVal'
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
            [ ("agora:governorPolicy", toRoledScript govPol')
            , ("agora:governorValidator", toRoledScript govVal')
            , ("agora:stakePolicy", toRoledScript stakPol')
            , ("agora:stakeValidator", toRoledScript stakVal')
            , ("agora:proposalPolicy", toRoledScript propPol')
            , ("agora:proposalValidator", toRoledScript propVal')
            , ("agora:treasuryValidator", toRoledScript treaVal')
            , ("agora:authorityTokenPolicy", toRoledScript atPol')
            , ("agora:noOpValidator", toRoledScript noOpVal')
            , ("agora:treasuryWithdrawalValidator", toRoledScript treaWithdrawalVal')
            , ("agora:mutateGovernorValidator", toRoledScript mutateGovVal')
            , ("agora:alwaysSucceedsPolicy", toRoledScript alwaysSucceedsPolicy')
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
