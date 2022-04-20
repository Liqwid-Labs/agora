{- |
Module     : Agora.AuthorityToken
Maintainer : emi@haskell.fyi
Description: Tokens acting as redeemable proofs of DAO authority.

Tokens acting as redeemable proofs of DAO authority.
-}
module Agora.AuthorityToken (
  authorityTokenPolicy,
  authorityTokensValidIn,
  singleAuthorityTokenBurned,
  AuthorityToken (..),
) where

import Plutarch.Api.V1 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol (..),
  PScriptContext (..),
  PScriptPurpose (..),
  PTxInInfo (PTxInInfo),
  PTxInfo (..),
  PTxOut (..),
  PMintingPolicy
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Builtin (pforgetData)
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf)

import Prelude

--------------------------------------------------------------------------------

import Agora.Utils (
  allOutputs,
  passert,
  plookup,
  psymbolValueOf,
  ptokenSpent,
 )

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

{- | Check that all GATs are valid in a particular TxOut.
     How this is checked: an AuthorityToken should never leave
     the Effect it was initially sent to, so we simply check that
     the script address the token resides in matches the TokenName.
     Since the TokenName was tagged upon mint with the Effect script
     it was sent to, this is enough to prove validity.

     In other words, check that all assets of a particular currency symbol
     are tagged with a TokenName that matches where they live.
-}
authorityTokensValidIn :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
authorityTokensValidIn = phoistAcyclic $
  plam $ \authorityTokenSym txOut'' -> P.do
    PTxOut txOut' <- pmatch txOut''
    txOut <- pletFields @'["address", "value"] $ txOut'
    PAddress address <- pmatch txOut.address
    PValue value' <- pmatch txOut.value
    PMap value <- pmatch value'
    pmatch (plookup # pdata authorityTokenSym # value) $ \case
      PJust (pfromData -> tokenMap') ->
        pmatch (pfield @"credential" # address) $ \case
          PPubKeyCredential _ ->
            -- GATs should only be sent to Effect validators
            ptraceIfFalse "authorityTokensValidIn: GAT incorrectly lives at PubKey" $ pconstant False
          PScriptCredential ((pfromData . (pfield @"_0" #)) -> cred) -> P.do
            PMap tokenMap <- pmatch tokenMap'
            ptraceIfFalse "authorityTokensValidIn: GAT TokenName doesn't match ScriptHash" $
              pall
                # plam
                  ( \pair ->
                      pforgetData (pfstBuiltin # pair) #== pforgetData (pdata cred)
                  )
                # tokenMap
      PNothing ->
        -- No GATs exist at this output!
        pconstant True

-- | Assert that a single authority token has been burned.
singleAuthorityTokenBurned ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s (PAsData PTxInfo) ->
  Term s PValue ->
  Term s PBool
singleAuthorityTokenBurned gatCs txInfo mint = P.do
  let gatAmountMinted :: Term _ PInteger
      gatAmountMinted = psymbolValueOf # gatCs # mint

  txInfoF <- pletFields @'["inputs"] $ txInfo

  foldr1
    (#&&)
    [ ptraceIfFalse "singleAuthorityTokenBurned: Must burn exactly 1 GAT" $ gatAmountMinted #== -1
    , ptraceIfFalse "singleAuthorityTokenBurned: All GAT tokens must be valid at the inputs" $
        pall
          # plam
            ( \txInInfo' -> P.do
                PTxInInfo txInInfo <- pmatch (pfromData txInInfo')
                let txOut' = pfield @"resolved" # txInInfo
                authorityTokensValidIn # gatCs # pfromData txOut'
            )
          # txInfoF.inputs
    ]

-- | Policy given 'AuthorityToken' params.
authorityTokenPolicy :: AuthorityToken -> ClosedTerm PMintingPolicy
authorityTokenPolicy params =
  plam $ \_redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx') -> P.do
      ctx <- pletFields @'["txInfo", "purpose"] ctx'
      PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
      txInfo <- pletFields @'["inputs", "mint"] txInfo'
      let inputs = txInfo.inputs
          mintedValue = pfromData txInfo.mint
          AssetClass (govCs, govTn) = params.authority
          govAc = passetClass # pconstant govCs # pconstant govTn
          govTokenSpent = ptokenSpent # govAc # inputs

      PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose

      let ownSymbol = pfromData $ pfield @"_0" # ownSymbol'
          mintedATs = passetClassValueOf # mintedValue # (passetClass # ownSymbol # pconstant "")
      pif
        (0 #< mintedATs)
        ( P.do
            passert "Parent token did not move in minting GATs" govTokenSpent
            passert "All outputs only emit valid GATs" $
              allOutputs @PData # pfromData ctx.txInfo #$ plam $ \txOut _value _address _datum ->
                authorityTokensValidIn
                  # ownSymbol
                  # txOut
            popaque $ pconstant ()
        )
        (popaque $ pconstant ())
