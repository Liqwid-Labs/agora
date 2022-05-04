{-# OPTIONS_GHC -Wwarn #-}

{- |
Module: Spec.Treasury
Description: Tests for Agora treasury.
Maintainer: jack@mlabs.city

This module exports `tests`, a list of `TestTree`s, which ensure
that Agora's treasury component works as desired.
-}
module Spec.Treasury (tests) where

import Agora.Treasury (
  TreasuryRedeemer (SpendTreasuryGAT),
  treasuryValidator,
 )
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Spending),
  TxInfo (txInfoMint),
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import Spec.Sample.Treasury (
  BadTreasuryRedeemer (NukeTheSystem),
  gatCs,
  gatTn,
  treasuryRef,
  validCtx,
 )
import Spec.Util (validatorFailsWith, validatorSucceedsWith)
import Test.Tasty (TestTree, testGroup)

{-

`Spec.Util` provides a number of useful functions:

  - policySucceedsWith: checks that a minting policy succeeds.

  - policyFailsWith: checks that a minting policy fails.

  - validatorSucceedsWith: checks that validator succeeds.

  - validatorFailsWith: checks that validator fails.

  - scriptSucceeds: checks that an arbitrary script does not
    `perror`.

  - scriptFails: checks that an arbitrary script `perror`s out.

-}

{-

Tests need to fail when:

  1. The reedeemer is of inproper form.
  2. The script purpose is not minting.
  3. `singleAuthorityTokenBurned` returns false.
    a. Multiple GATs burned.
    b. An input returns 'False' for 'authorityTokensValidIn'

-}

tests :: [TestTree]
tests =
  [ testGroup
      "validator"
      [ validatorSucceedsWith
          "Allows for effect changes"
          (treasuryValidator gatCs)
          ()
          SpendTreasuryGAT
          validCtx
      , validatorFailsWith
          "Fails with invalid redeemer"
          (treasuryValidator gatCs)
          ()
          (NukeTheSystem)
          validCtx
      , validatorFailsWith
          "Fails with ScriptPurpose not Minting"
          (treasuryValidator gatCs)
          ()
          SpendTreasuryGAT
          validCtx
            { scriptContextPurpose = Spending treasuryRef
            }
      , validatorFailsWith
          "Fails when multiple GATs burned"
          (treasuryValidator gatCs)
          ()
          SpendTreasuryGAT
          validCtx
            { scriptContextTxInfo =
                validCtx.scriptContextTxInfo
                  { txInfoMint = Value.singleton gatCs gatTn (-2)
                  }
            }
      ]
  ]
