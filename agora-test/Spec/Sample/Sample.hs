module Spec.Sample.Sample () where

import Test.QuickCheck 
import Plutus.V1.Ledger.Api

import Data.ByteString.Char8 qualified as C
import Data.ByteString.Hash (sha2)

-- TODO: How do I need to ensure uniqueness?
usersGen :: Gen Credential
usersGen =
  (chooseAny :: Gen Integer) >>= pure . PubKeyCredential . PubKeyHash . toBuiltin . sha2 . C.pack . show

treasuriesGen :: Gen Credential
treasuriesGen =
  (chooseAny :: Gen Integer) >>= pure . ScriptCredential . ValidatorHash . toBuiltin . sha2 . C.pack . show

txInInfoGen :: Gen TxInInfo
txInInfoGen = do
  undefined
