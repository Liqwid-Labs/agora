{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Agora.Linker (linker, AgoraScriptInfo (..)) where

import Agora.Governor (Governor (gstOutRef, gtClassRef, maximumCosigners))
import Agora.SafeMoney (AuthorityTokenTag, GTTag, GovernorSTTag, ProposalSTTag, StakeSTTag)
import Data.Aeson qualified as Aeson
import Data.Map (fromList)
import Data.Tagged (Tagged (Tagged))
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), ScriptHash, TxOutRef, getScriptHash)
import Ply (
  AsData (AsData),
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
      @'[AsData TxOutRef]
      "agora:governorPolicy"
  govVal <-
    fetchTS
      @ValidatorRole
      @'[ AsData ScriptHash
        , AsData (Tagged StakeSTTag AssetClass)
        , AsData (Tagged GovernorSTTag CurrencySymbol)
        , AsData (Tagged ProposalSTTag CurrencySymbol)
        , AsData (Tagged AuthorityTokenTag CurrencySymbol)
        ]
      "agora:governorValidator"
  stkPol <-
    fetchTS
      @MintingPolicyRole
      @'[AsData (Tagged GTTag AssetClass)]
      "agora:stakePolicy"
  stkVal <-
    fetchTS
      @ValidatorRole
      @'[ AsData (Tagged StakeSTTag CurrencySymbol)
        , AsData (Tagged ProposalSTTag AssetClass)
        , AsData (Tagged GTTag AssetClass)
        ]
      "agora:stakeValidator"
  prpPol <-
    fetchTS @MintingPolicyRole
      @'[AsData (Tagged GovernorSTTag AssetClass)]
      "agora:proposalPolicy"
  prpVal <-
    fetchTS
      @ValidatorRole
      @'[ AsData (Tagged StakeSTTag AssetClass)
        , AsData (Tagged GovernorSTTag CurrencySymbol)
        , AsData (Tagged ProposalSTTag CurrencySymbol)
        , AsData Integer
        ]
      "agora:proposalValidator"
  treVal <-
    fetchTS
      @ValidatorRole
      @'[AsData (Tagged AuthorityTokenTag CurrencySymbol)]
      "agora:treasuryValidator"
  atkPol <-
    fetchTS
      @MintingPolicyRole
      @'[AsData (Tagged GovernorSTTag AssetClass)]
      "agora:authorityTokenPolicy"
  noOpVal <-
    fetchTS
      @ValidatorRole
      @'[AsData (Tagged AuthorityTokenTag CurrencySymbol)]
      "agora:noOpValidator"
  treaWithdrawalVal <-
    fetchTS
      @ValidatorRole
      @'[AsData (Tagged AuthorityTokenTag CurrencySymbol)]
      "agora:treasuryWithdrawalValidator"
  mutateGovVal <-
    fetchTS
      @ValidatorRole
      @'[ AsData ScriptHash
        , AsData (Tagged GovernorSTTag CurrencySymbol)
        , AsData (Tagged AuthorityTokenTag CurrencySymbol)
        ]
      "agora:mutateGovernorValidator"

  governor <- getParam

  let govPol' = govPol # AsData governor.gstOutRef
      govVal' =
        govVal
          # AsData propValHash
          # AsData (Tagged sstAssetClass)
          # AsData (Tagged gstSymbol)
          # AsData (Tagged pstSymbol)
          # AsData (Tagged atSymbol)
      gstSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript govPol'
      gstAssetClass =
        AssetClass gstSymbol ""
      govValHash = scriptHash $ toScript govVal'

      atPol' = atkPol # AsData (Tagged gstAssetClass)
      atSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript atPol'

      propPol' = prpPol # AsData (Tagged gstAssetClass)
      propVal' =
        prpVal
          # AsData (Tagged sstAssetClass)
          # AsData (Tagged gstSymbol)
          # AsData (Tagged pstSymbol)
          # AsData governor.maximumCosigners
      propValHash = scriptHash $ toScript propVal'
      pstSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript propPol'
      pstAssetClass = AssetClass pstSymbol ""

      stakPol' = stkPol # AsData governor.gtClassRef
      stakVal' =
        stkVal
          # AsData (Tagged sstSymbol)
          # AsData (Tagged pstAssetClass)
          # AsData governor.gtClassRef
      sstSymbol = CurrencySymbol . getScriptHash . scriptHash $ toScript stakPol'
      stakValTokenName =
        scriptHashToTokenName $ scriptHash $ toScript stakVal'
      sstAssetClass = AssetClass sstSymbol stakValTokenName

      treaVal' = treVal # AsData (Tagged atSymbol)

      noOpVal' = noOpVal # AsData (Tagged atSymbol)
      treaWithdrawalVal' = treaWithdrawalVal # AsData (Tagged atSymbol)
      mutateGovVal' =
        mutateGovVal
          # AsData govValHash
          # AsData (Tagged gstSymbol)
          # AsData (Tagged atSymbol)

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
