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
  treasuryValidator,
 )
import Agora.Utils (CompiledValidator (CompiledValidator))
import Plutarch.Api.V2 (mkValidator)
import PlutusLedgerApi.V1.Credential (
  StakingCredential (StakingHash),
 )
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2 (DCert (DCertDelegRegKey))
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Certifying, Minting, Rewarding),
  TxInfo (txInfoInputs, txInfoMint),
 )
import Sample.Shared (deterministicTracingConfing, trCredential)
import Sample.Treasury (
  gatCs,
  gatTn,
  trCtxGATNameNotAddress,
  validCtx,
  walletIn,
 )
import Test.Specification (
  SpecificationTree,
  group,
  validatorFailsWith,
  validatorSucceedsWith,
 )

compiledTreasuryValidator :: CompiledValidator () ()
compiledTreasuryValidator =
  CompiledValidator $
    mkValidator deterministicTracingConfing $
      treasuryValidator gatCs

specs :: [SpecificationTree]
specs =
  [ group
      "Validator"
      [ group
          "Positive"
          [ validatorSucceedsWith
              "Allows for effect changes"
              compiledTreasuryValidator
              ()
              ()
              validCtx
          , validatorSucceedsWith
              "Fails when GAT token name is not script address"
              compiledTreasuryValidator
              ()
              ()
              trCtxGATNameNotAddress
          ]
      , group
          "Negative"
          [ group
              "Fails with ScriptPurpose not Spending"
              [ validatorFailsWith
                  "Minting"
                  compiledTreasuryValidator
                  ()
                  ()
                  validCtx
                    { scriptContextPurpose = Minting ""
                    }
              , validatorFailsWith
                  "Rewarding"
                  compiledTreasuryValidator
                  ()
                  ()
                  validCtx
                    { scriptContextPurpose =
                        Rewarding $
                          StakingHash trCredential
                    }
              , validatorFailsWith
                  "Certifying"
                  compiledTreasuryValidator
                  ()
                  ()
                  validCtx
                    { scriptContextPurpose =
                        Certifying $
                          DCertDelegRegKey $
                            StakingHash trCredential
                    }
              ]
          , validatorFailsWith -- TODO: Use QuickCheck.
              "Fails when multiple GATs burned"
              compiledTreasuryValidator
              ()
              ()
              validCtx
                { scriptContextTxInfo =
                    (getField @"scriptContextTxInfo" validCtx)
                      { txInfoMint =
                          Value.singleton
                            gatCs
                            gatTn
                            (-2)
                      }
                }
          , validatorFailsWith
              "Fails with wallet as input"
              compiledTreasuryValidator
              ()
              ()
              ( let txInfo = getField @"scriptContextTxInfo" validCtx
                    inputs = getField @"txInfoInputs" txInfo
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
