{- |
Module: Main
Description: Agora test suite.
Maintainer: emi@haskell.fyi

This module is the root of Agora's test suite.
-}
module Main (main) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Spec.AuthorityToken qualified as AuthorityToken
import Spec.Effect.TreasuryWithdrawal qualified as TreasuryWithdrawal
import Spec.Model.MultiSig qualified as MultiSig
import Spec.Proposal qualified as Proposal
import Spec.Stake qualified as Stake
import Spec.Treasury qualified as Treasury

-- | The Agora test suite.
main :: IO ()
main =
  defaultMain $
    testGroup
      "test suite"
      [ testGroup
          "Effects"
          [ testGroup
              "Treasury Withdrawal Effect"
              TreasuryWithdrawal.tests
          ]
      , testGroup
          "Stake tests"
          Stake.tests
      , testGroup
          "Proposal tests"
          Proposal.tests
      , testGroup
          "Multisig tests"
          [ testGroup
              "MultiSig"
              [ MultiSig.plutarchTests
              , MultiSig.genTests
              ]
          ]
      , testGroup
          "AuthorityToken tests"
          AuthorityToken.tests
      , testGroup
          "Treasury tests"
          Treasury.tests
      ]
