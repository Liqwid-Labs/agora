{- |
Module     : Agora.MultiSig
Maintainer : riley_kilgore@outlook.com
Description: A basic N of M multisignature validation function.
-}
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
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P

import Plutus.V1.Ledger.Crypto (PubKeyHash)

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

--------------------------------------------------------------------------------
pubKeysToPPubKeys ::
  forall s. [PubKeyHash] -> PBuiltinList (PAsData PPubKeyHash) s
pubKeysToPPubKeys xs = case xs of
  x:xs' ->
    let x' :: Term s (PAsData PPubKeyHash)
        x' = pconstantData x
    in
    PCons x' (pcon $ pubKeysToPPubKeys xs')
  []    ->
    PNil

multisigToPMultisig :: forall s. MultiSig -> Term s PMultiSig
multisigToPMultisig m =
    pcon $ PMultiSig (pdcons @"keys" @(PBuiltinList (PAsData PPubKeyHash))
      # (pdata $ pcon (pubKeysToPPubKeys m.keys))
      #$ pdcons @"minSigs" @PInteger # (pdata $ fromInteger m.minSigs) # pdnil)

validatedByMultisig :: MultiSig -> Term s (PScriptContext :--> PBool)
validatedByMultisig params =
  plam $ \ctx' -> P.do
    pvalidatedByMultisig # (multisigToPMultisig params) # ctx'

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