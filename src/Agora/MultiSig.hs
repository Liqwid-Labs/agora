{- |
Module     : Agora.MultiSig
Maintainer : riley_kilgore@outlook.com
Description: A basic N of M multisignature validator.
-}
module Agora.MultiSig (
  multiSigValidator,
  MultiSig (..),
) where

import Plutarch.Api.V1 (
  PPubKeyHash,
  PScriptContext (..),
 )
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Agora.Utils (passert)

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)
import Prelude

--------------------------------------------------------------------------------

{- | A MultiSig represents a proof that a particular set of signatures
     are present on a transaction.
-}
data MultiSig (s :: S) = MultiSig
  { keys    :: PList PPubKeyHash s
  -- ^ List of PubKeyHashes that must be present in the list of signatories.
  , minSigs :: Integer
  } deriving stock (GHC.Generic)
    deriving anyclass (Generic)

--------------------------------------------------------------------------------

-- | Validator given 'MultiSig' params.
multiSigValidator :: MultiSig s -> Term s (PData :--> PData :--> PScriptContext :--> PUnit)
multiSigValidator params =
  plam $ \_datum _redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    let signatories = pfield @"signatories" # ctx.txInfo
    passert "The amount of required signatures is not met."
      ((fromInteger params.minSigs) #<= (plength #$ pfilter
          # (plam $ \a ->
              (pelem # pdata a # pfromData signatories))
          # pcon params.keys))
      (pconstant ())