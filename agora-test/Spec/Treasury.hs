{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Spec.Treasury
Description: Tests for Agora treasury.
Maintainer: jack@mlabs.city

This module exports `tests`, a list of `TestTree`s, which ensure
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
module Spec.Treasury (tests) where

import Agora.Treasury (
  TreasuryRedeemer (SpendTreasuryGAT),
  treasuryValidator,
 )
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  DCert (DCertDelegRegKey),
 )
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Certifying, Rewarding, Spending),
  TxInfo (txInfoInputs, txInfoMint),
  txInInfoResolved,
  txOutAddress,
 )
import Plutus.V1.Ledger.Credential (
  Credential (ScriptCredential),
  StakingCredential (StakingHash),
 )
import Plutus.V1.Ledger.Scripts (
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Value qualified as Value
import Spec.Sample.Treasury (
  -- BadTreasuryRedeemer (NukeTheSystem),
  gatCs,
  gatTn,
  trCredential,
  treasuryRef,
  validCtx,
  walletIn,
 )
import Spec.Util (validatorFailsWith, validatorSucceedsWith)
import Test.Tasty (TestTree, testGroup)

tests :: [TestTree]
tests =
  [ testGroup
      "Validator"
      [ testGroup
          "Positive"
          [ validatorSucceedsWith
              "Allows for effect changes"
              (treasuryValidator gatCs)
              ()
              SpendTreasuryGAT
              validCtx
          ]
      , testGroup
          "Negative"
          [ testGroup
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
          , -- , validatorFailsWith -- TODO: Check.
            --     "Fails with invalid redeemer"
            --     (treasuryValidator gatCs)
            --     ()
            --     (NukeTheSystem 72)
            --     validCtx

            validatorFailsWith -- TODO: Use QuickCheck.
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
              ( let txInfo = validCtx.scriptContextTxInfo
                    inputs = txInfo.txInfoInputs
                    effectIn = inputs !! 1
                    invalidEff =
                      effectIn
                        { txInInfoResolved =
                            effectIn.txInInfoResolved
                              { txOutAddress =
                                  Address
                                    ( ScriptCredential $
                                        ValidatorHash
                                          wrongHash
                                    )
                                    Nothing
                              }
                        }
                 in validCtx
                      { scriptContextTxInfo =
                          txInfo
                            { txInfoInputs =
                                [ head inputs
                                , invalidEff
                                ]
                            }
                      }
              )
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

{- | A SHA-256 hash which (in all certainty) should not match the
     hash of the dummy effect script.
-}
wrongHash :: BuiltinByteString
wrongHash = "a21bc4a1d95600f9fa0a00b97ed0fa49a152a72de76253cb706f90b4b40f837b"
