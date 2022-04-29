{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.MultiSig
Maintainer : riley_kilgore@outlook.com
Description: A basic N of M multisignature validation function.

A basic N of M multisignature validation function.
-}
module Agora.MultiSig (
  validatedByMultisig,
  pvalidatedByMultisig,
  PMultiSig (..),
  MultiSig (..),
) where

import Plutarch.Api.V1 (
  PPubKeyHash,
  PTxInfo (..),
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Monadic qualified as P

import Plutus.V1.Ledger.Crypto (PubKeyHash)
import PlutusTx qualified

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Prelude

--------------------------------------------------------------------------------

{- | A MultiSig represents a proof that a particular set of signatures
     are present on a transaction.
-}
data MultiSig = MultiSig
  { keys :: [PubKeyHash]
  -- ^ List of PubKeyHashes that must be present in the list of signatories.
  , minSigs :: Integer
  }
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (Generic)

PlutusTx.makeLift ''MultiSig
PlutusTx.unstableMakeIsData ''MultiSig

-- | Plutarch-level MultiSig
newtype PMultiSig (s :: S) = PMultiSig
  { getMultiSig ::
    Term
      s
      ( PDataRecord
          '[ "keys" ':= PBuiltinList (PAsData PPubKeyHash)
           , "minSigs" ':= PInteger
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PMultiSig)

instance PUnsafeLiftDecl PMultiSig where type PLifted PMultiSig = MultiSig
deriving via (DerivePConstantViaData MultiSig PMultiSig) instance (PConstantDecl MultiSig)

--------------------------------------------------------------------------------

-- | Check if a Haskell-level MultiSig signs this transaction.
validatedByMultisig :: MultiSig -> Term s (PTxInfo :--> PBool)
validatedByMultisig params =
  phoistAcyclic $
    pvalidatedByMultisig # pconstant params

-- | Check if a Plutarch-level MultiSig signs this transaction.
pvalidatedByMultisig :: Term s (PMultiSig :--> PTxInfo :--> PBool)
pvalidatedByMultisig =
  phoistAcyclic $
    plam $ \multi' txInfo -> P.do
      multi <- pletFields @'["keys", "minSigs"] multi'
      let signatories = pfield @"signatories" # txInfo
      pfromData multi.minSigs
        #<= ( plength #$ pfilter
                # plam
                  ( \a ->
                      pelem # a # pfromData signatories
                  )
                # multi.keys
            )
