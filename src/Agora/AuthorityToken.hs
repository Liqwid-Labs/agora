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

{- | An AuthorityToken represents a proof that a particular token moved while this token was minted.
 In effect, this means that the validator that locked such a token must have approved said transaction.
 Said validator should be made aware of _this_ token's existence in order to prevent incorrect minting.
-}
data AuthorityToken = AuthorityToken
  { -- | Token that must move in order for minting this to be valid.
    authority :: AssetClass
  }

--------------------------------------------------------------------------------

authorityTokenPolicy :: AuthorityToken -> Term s (PData :--> PData :--> PScriptContext :--> PUnit)
authorityTokenPolicy _params =
  plam $ \_datum _redeemer _ctx ->
    pif (pcon PTrue) (pcon PUnit) (ptraceError "Constraint failed")
