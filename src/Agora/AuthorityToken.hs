{-# LANGUAGE PolyKinds #-}

module Agora.AuthorityToken (
  authorityTokenPolicy,
  AuthorityToken (..),
) where

import Plutarch.Api.V1 (
  PScriptContext (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.List (pfoldr')
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (AssetClass)

import Prelude

--------------------------------------------------------------------------------

import Agora.Utils (passetClassValueOf, passetClassValueOf')

--------------------------------------------------------------------------------

{- | An AuthorityToken represents a proof that a particular token
     moved while this token was minted. In effect, this means that
     the validator that locked such a token must have approved
     said transaction. Said validator should be made aware of
     _this_ token's existence in order to prevent incorrect minting.
-}
newtype AuthorityToken = AuthorityToken
  { authority :: AssetClass
  -- ^ Token that must move in order for minting this to be valid.
  }

--------------------------------------------------------------------------------

authorityTokenPolicy ::
  AuthorityToken ->
  Term s (PData :--> PScriptContext :--> PUnit)
authorityTokenPolicy params =
  plam $ \_redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx') -> P.do
      ctx <- pletFields @'["txInfo", "purpose"] ctx'
      PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
      txInfo <- pletFields @'["inputs", "mint"] txInfo'
      let inputs = txInfo.inputs :: Term _ (PBuiltinList (PAsData PTxInInfo))
      let authorityTokenInputs =
            pfoldr'
              ( \txInInfo' acc -> P.do
                  PTxInInfo txInInfo <- pmatch (pfromData txInInfo')
                  PTxOut txOut' <- pmatch $ pfromData $ pfield @"resolved" # txInInfo
                  txOut <- pletFields @'["value"] txOut'
                  let txOutValue = pfromData txOut.value
                  passetClassValueOf' params.authority # txOutValue + acc
              )
              # 0
              # inputs
      let mintedValue = pfromData txInfo.mint
      let tokenMoved = 0 #< authorityTokenInputs
      PMinting sym' <- pmatch $ pfromData ctx.purpose
      let sym = pfromData $ pfield @"_0" # sym'
      let mintedATs = passetClassValueOf # sym # pconstant "" # mintedValue
      pif
        (0 #< mintedATs)
        ( pif
            tokenMoved
            -- The authority token moved, we are good to go for minting.
            (pconstant ())
            (ptraceError "Authority token did not move in minting GATs")
        )
        -- We minted 0 or less Authority Tokens, we are good to go.
        -- Burning is always allowed.
        (pconstant ())
