module Agora.AuthorityToken (authorityTokenPolicy, AuthorityToken (..)) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Value (AssetClass)

--------------------------------------------------------------------------------

import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.ScriptContext
import Plutarch.Trace
import Plutarch.Unit (PUnit (..))

--------------------------------------------------------------------------------

data AuthorityToken = AuthorityToken
  { -- | Token that must move in order for minting this to be valid.
    authorityAssetClass :: AssetClass
  }

--------------------------------------------------------------------------------

authorityTokenPolicy :: AuthorityToken -> Term s (PData :--> PData :--> PScriptContext :--> PUnit)
authorityTokenPolicy _params =
  plam $ \_datum _redeemer _ctx ->
    pif (pcon PTrue) (pcon PUnit) (ptraceError "Constraint failed")
