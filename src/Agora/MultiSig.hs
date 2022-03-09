{- |
Module     : Agora.MultiSig
Maintainer : riley_kilgore@outlook.com
Description: A basic N of M multisignature validation function.
-}
{-# LANGUAGE TemplateHaskell     #-}

module Agora.MultiSig (
  validatedByMultisig,
  pvalidatedByMultisig,
  MultiSig (..),
) where

import Plutarch.Api.V1 (
  PPubKeyHash,
  PScriptContext (..),
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Monadic qualified as P

import Plutus.V1.Ledger.Crypto (PubKeyHash)
import qualified PlutusTx

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Prelude

--------------------------------------------------------------------------------

{- | A MultiSig represents a proof that a particular set of signatures
     are present on a transaction.
-}
data MultiSig = MultiSig
  { keys    :: [PubKeyHash]
  -- ^ List of PubKeyHashes that must be present in the list of signatories.
  , minSigs :: Integer
  } deriving stock (GHC.Generic)
    deriving anyclass (Generic)

PlutusTx.makeLift ''MultiSig
PlutusTx.unstableMakeIsData ''MultiSig

newtype PMultiSig (s :: S) = PMultiSig
  { getMultiSig ::
    Term s (PDataRecord '[ "keys" ':= PBuiltinList (PAsData PPubKeyHash)
                         , "minSigs" ':= PInteger])
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PMultiSig)

instance PUnsafeLiftDecl PMultiSig where type PLifted PMultiSig = MultiSig
deriving via (DerivePConstantViaData MultiSig PMultiSig) instance (PConstant MultiSig)

--------------------------------------------------------------------------------

validatedByMultisig :: MultiSig -> Term s (PScriptContext :--> PBool)
validatedByMultisig params =
  plam $ \ctx' -> P.do
    pvalidatedByMultisig # (pconstant params) # ctx'

pvalidatedByMultisig :: Term s (PMultiSig :--> PScriptContext :--> PBool)
pvalidatedByMultisig =
  plam $ \multi' ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    multi <- pletFields @'["keys", "minSigs"] multi'
    let signatories = pfield @"signatories" # ctx.txInfo
    ((pfromData multi.minSigs) #<= (plength #$ pfilter
        # (plam $ \a ->
            (pelem # a # pfromData signatories))
        # multi.keys))