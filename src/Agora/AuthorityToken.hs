{- |
Module     : Agora.AuthorityToken
Maintainer : emi@haskell.fyi
Description: Tokens acting as redeemable proofs of DAO authority
-}
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

import Agora.Utils (passert, passetClassValueOf, passetClassValueOf')

--------------------------------------------------------------------------------

{- | An AuthorityToken represents a proof that a particular token
     moved while this token was minted. In effect, this means that
     the validator that locked such a token must have approved
     said transaction. Said validator should be made aware of
     *this* token's existence in order to prevent incorrect minting.
-}
newtype AuthorityToken = AuthorityToken
  { authority :: AssetClass
  -- ^ Token that must move in order for minting this to be valid.
  }

--------------------------------------------------------------------------------

-- | Policy given 'AuthorityToken' params.
authorityTokenPolicy ::
  AuthorityToken ->
  Term s (PData :--> PScriptContext :--> PUnit)
authorityTokenPolicy params =
  plam $ \_redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx') -> P.do
      ctx <- pletFields @'["txInfo", "purpose"] ctx'
      PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
      txInfo <- pletFields @'["inputs", "mint"] txInfo'
      let inputs = txInfo.inputs
      let authorityTokenInputs =
            pfoldr' @PBuiltinList
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
      PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
      let ownSymbol = pfromData $ pfield @"_0" # ownSymbol'
      let mintedATs = passetClassValueOf # ownSymbol # pconstant "" # mintedValue
      pif
        (0 #< mintedATs)
        (passert "Authority token did not move in minting GATs" tokenMoved (pconstant ()))
        (pconstant ())
