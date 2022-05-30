{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Spec.Treasury
Description: Tests for Agora treasury.
Maintainer: jack@mlabs.city

This module exports `specs`, a list of `TestTree`s, which ensure
that Agora's treasury component works as desired.

Tests need to fail when:

  1. The reedeemer is of inproper form. TODO: Inquire.
  2. The script purpose is not minting.
  3. `singleAuthorityTokenBurned` returns false.
    a. @n /= -1@ GATs burned.
    b. An input returns 'False' for 'authorityTokensValidIn'
      i. A wallet input has a GAT.
      ii. A script has a GAT, the token name for which does /not/
          match the script's validator hash.
-}
module Spec.Treasury (specs) where

import Agora.Treasury (
  TreasuryRedeemer (SpendTreasuryGAT),
  treasuryValidator,
 )
import Plutus.V1.Ledger.Api (
  DCert (DCertDelegRegKey),
 )
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Certifying, Rewarding, Spending),
  TxInfo (txInfoInputs, txInfoMint),
 )
import Plutus.V1.Ledger.Credential (
  StakingCredential (StakingHash),
 )
import Plutus.V1.Ledger.Value qualified as Value
import Sample.Shared (
  trCredential,
 )
import Sample.Treasury (
  gatCs,
  gatTn,
  trCtxGATNameNotAddress,
  treasuryRef,
  validCtx,
  walletIn,
 )
import Test.Specification (
  SpecificationTree,
  group,
  validatorFailsWith,
  validatorSucceedsWith,
 )

specs :: [SpecificationTree]
specs =
  [ group
      "Validator"
      [ group
          "Positive"
          [ validatorSucceedsWith
              "Allows for effect changes"
              (treasuryValidator gatCs)
              ()
              SpendTreasuryGAT
              validCtx
          ]
      , group
          "Negative"
          [ group
              "Fails with ScriptPurpose not Minting"
              [ validatorFailsWith
                  "Spending"
                  (treasuryValidator gatCs)
                  ()
                  SpendTreasuryGAT
                  validCtx
                    { scriptContextPurpose = Spending treasuryRef
                    }
              , validatorFailsWith
                  "Rewarding"
                  (treasuryValidator gatCs)
                  ()
                  SpendTreasuryGAT
                  validCtx
                    { scriptContextPurpose =
                        Rewarding $
                          StakingHash trCredential
                    }
              , validatorFailsWith
                  "Certifying"
                  (treasuryValidator gatCs)
                  ()
                  SpendTreasuryGAT
                  validCtx
                    { scriptContextPurpose =
                        Certifying $
                          DCertDelegRegKey $
                            StakingHash trCredential
                    }
              ]
          , validatorFailsWith -- TODO: Use QuickCheck.
              "Fails when multiple GATs burned"
              (treasuryValidator gatCs)
              ()
              SpendTreasuryGAT
              validCtx
                { scriptContextTxInfo =
                    validCtx.scriptContextTxInfo
                      { txInfoMint =
                          Value.singleton
                            gatCs
                            gatTn
                            (-2)
                      }
                }
          , validatorFailsWith
              "Fails when GAT token name is not script address"
              (treasuryValidator gatCs)
              ()
              SpendTreasuryGAT
              trCtxGATNameNotAddress
          , validatorFailsWith
              "Fails with wallet as input"
              (treasuryValidator gatCs)
              ()
              SpendTreasuryGAT
              ( let txInfo = validCtx.scriptContextTxInfo
                    inputs = txInfo.txInfoInputs
                    newInputs =
                      [ head inputs
                      , walletIn
                      ]
                 in validCtx
                      { scriptContextTxInfo =
                          txInfo
                            { txInfoInputs = newInputs
                            }
                      }
              )
          ]
      ]
  ]
