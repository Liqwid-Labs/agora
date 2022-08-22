{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.AuthorityToken
Maintainer : emi@haskell.fyi
Description: Tests for Authority token functions

Tests for Authority token functions
-}
module Spec.AuthorityToken (specs) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Plutarch (ClosedTerm, POpaque, perror, popaque)
import Plutarch.Extra.Compile (mustCompile)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 (
  Address (Address),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  Script,
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  ValidatorHash (ValidatorHash),
  Value,
 )
import PlutusLedgerApi.V1.Value qualified as Value (
  Value (Value),
  singleton,
 )
import PlutusTx.AssocMap qualified as AssocMap (empty)
import Test.Specification (
  SpecificationTree,
  group,
  scriptFails,
  scriptSucceeds,
 )
import Prelude (
  Maybe (Nothing),
  PBool,
  Semigroup ((<>)),
  fmap,
  pconstant,
  pif,
  ($),
 )

currencySymbol :: CurrencySymbol
currencySymbol = "deadbeef"

mkInputs :: [TxOut] -> [TxInInfo]
mkInputs = fmap (TxInInfo (TxOutRef "" 0))

singleAuthorityTokenBurnedTest :: Value -> [TxOut] -> Script
singleAuthorityTokenBurnedTest mint outs =
  let actual :: ClosedTerm PBool
      actual = singleAuthorityTokenBurned (pconstant currencySymbol) (punsafeCoerce $ pconstant $ mkInputs outs) (pconstant mint)
      s :: ClosedTerm POpaque
      s =
        pif
          actual
          (popaque (pconstant ()))
          perror
   in mustCompile s

-- | The SpecificationTree exported by this module.
specs :: [SpecificationTree]
specs =
  [ -- This is better suited for plutarch-test
    group
      "singleAuthorityTokenBurned"
      [ scriptSucceeds
          "Correct simple"
          ( singleAuthorityTokenBurnedTest
              ( Value.singleton currencySymbol "deadbeef" (-1)
                  <> Value.singleton "aa" "USDC" 100_000
              )
              [ TxOut
                  (Address (ScriptCredential (ValidatorHash "deadbeef")) Nothing)
                  (Value.singleton currencySymbol "deadbeef" 1)
                  Nothing
              ]
          )
      , scriptSucceeds
          "Correct many inputs"
          ( singleAuthorityTokenBurnedTest
              ( Value.singleton currencySymbol "deadbeef" (-1)
                  <> Value.singleton "aa" "USDC" 100_000
              )
              [ TxOut
                  (Address (PubKeyCredential "") Nothing)
                  (Value.singleton "aaabcc" "hello-token" 1)
                  Nothing
              , TxOut
                  (Address (ScriptCredential (ValidatorHash "deadbeef")) Nothing)
                  (Value.singleton currencySymbol "deadbeef" 1)
                  Nothing
              , TxOut
                  (Address (PubKeyCredential "") Nothing)
                  (Value.singleton "" "" 1_000_000_000)
                  Nothing
              ]
          )
      , scriptSucceeds
          "Correct even though scripts don't match"
          ( singleAuthorityTokenBurnedTest
              ( Value.singleton currencySymbol "i'm not deadbeef!" (-1)
              )
              [ TxOut
                  (Address (ScriptCredential (ValidatorHash "deadbeef")) Nothing)
                  (Value.singleton currencySymbol "i'm not deadbeef!" 1)
                  Nothing
              ]
          )
      , scriptFails
          "Incorrect no burn"
          ( singleAuthorityTokenBurnedTest
              ( Value.Value AssocMap.empty
              )
              []
          )
      , scriptFails
          "Incorrect no GAT burn"
          ( singleAuthorityTokenBurnedTest
              ( Value.singleton "aabbcc" "not a GAT!" (-100)
              )
              []
          )
      , scriptFails
          "Incorrect spent from PK"
          ( singleAuthorityTokenBurnedTest
              ( Value.singleton currencySymbol "doesn't matter" (-1)
              )
              [ TxOut
                  (Address (PubKeyCredential "") Nothing)
                  (Value.singleton currencySymbol "doesn't matter" 1)
                  Nothing
              ]
          )
      , scriptFails
          "Incorrect two GATs"
          ( singleAuthorityTokenBurnedTest
              ( Value.singleton currencySymbol "deadbeef" (-2)
                  <> Value.singleton "aa" "USDC" 100_000
              )
              [ TxOut
                  (Address (ScriptCredential (ValidatorHash "deadbeef")) Nothing)
                  (Value.singleton currencySymbol "deadbeef" 2)
                  Nothing
              ]
          )
      ]
  ]
