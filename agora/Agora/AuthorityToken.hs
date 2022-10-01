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
) where

import Plutarch.Api.V1 (
  PCredential (..),
  PCurrencySymbol (..),
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PAddress (PAddress),
  PMintingPolicy,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
 )
import Plutarch.Extra.AssetClass (PAssetClass, passetClass, passetClassValueOf)
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)
import Plutarch.Extra.ScriptContext (pisTokenSpent)
import Plutarch.Extra.Sum (PSum (PSum))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Extra.Value (psymbolValueOf)

--------------------------------------------------------------------------------

{- | Check that all GATs are valid in a particular TxOut.
     How this is checked: an AuthorityToken should never leave
     the Effect it was initially sent to, so we simply check that
     the script address the token resides in matches the TokenName.
     Since the TokenName was tagged upon mint with the Effect script
     it was sent to, this is enough to prove validity.
     In other words, check that all assets of a particular currency symbol
     are tagged with a TokenName that matches where they live.

     As of version 1.0.0, this has been weakened in order to be compatible
     with RATs.

     @since 0.1.0
-}
authorityTokensValidIn :: forall (s :: S). Term s (PCurrencySymbol :--> PTxOut :--> PBool)
authorityTokensValidIn = phoistAcyclic $
  plam $ \authorityTokenSym txOut'' -> unTermCont $ do
    PTxOut txOut' <- pmatchC txOut''
    txOut <- pletFieldsC @'["address", "value"] $ txOut'
    PAddress address <- pmatchC txOut.address
    PValue value' <- pmatchC txOut.value
    PMap value <- pmatchC value'
    pure $
      pmatch (plookupAssoc # pfstBuiltin # psndBuiltin # pdata authorityTokenSym # value) $ \case
        PJust (pfromData -> _tokenMap') ->
          pmatch (pfield @"credential" # address) $ \case
            PPubKeyCredential _ ->
              -- GATs should only be sent to Effect validators
              ptraceIfFalse "authorityTokensValidIn: GAT incorrectly lives at PubKey" $ pconstant False
            PScriptCredential _ ->
              -- NOTE: We no longer can perform a check on `TokenName` content here.
              -- Instead, the auth check system uses `TokenName`s, but it cannot
              -- check for GATs incorrectly escaping scripts. The effect scripts
              -- need to be written very carefully in order to disallow this.
              pcon PTrue
        PNothing ->
          -- No GATs exist at this output!
          pcon PTrue

{- | Assert that a single authority token has been burned.

     @since 0.2.0
-}
singleAuthorityTokenBurned ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PValue keys amounts) ->
  Term s PBool
singleAuthorityTokenBurned gatCs inputs mint = unTermCont $ do
  let gatAmountMinted :: Term _ PInteger
      gatAmountMinted = psymbolValueOf # gatCs # mint

  let inputsWithGAT =
        pfoldMap
          # plam
            ( flip pmatch $ \case
                PTxInInfo txInInfo -> unTermCont $ do
                  resolved <- pletC $ pfield @"resolved" # txInInfo

                  pguardC "While counting GATs at inputs: all GATs must be valid" $
                    authorityTokensValidIn # gatCs
                      #$ pfromData
                      $ resolved

                  pure . pcon . PSum $
                    psymbolValueOf
                      # gatCs
                      #$ pfield @"value"
                      #$ resolved
            )
          # inputs
  pure $
    foldr1
      (#&&)
      [ ptraceIfFalse "singleAuthorityTokenBurned: Must burn exactly 1 GAT" $
          gatAmountMinted #== -1
      , ptraceIfFalse "Only one GAT must exist at the inputs" $
          inputsWithGAT #== 1
      ]

{- | Policy given 'AuthorityToken' params.

     == Authority Token

     An AuthorityToken represents a proof that a particular token
     spent in the same transaction the AuthorityToken was minted.
     In effect, this means that the validator that locked such a token
     must have approved the transaction in which an AuthorityToken is minted.
     Said validator should be made aware of an AuthorityToken token's existence
     in order to prevent incorrect minting.

     @since 0.1.0
-}
authorityTokenPolicy :: ClosedTerm (PAssetClass :--> PMintingPolicy)
authorityTokenPolicy =
  plam $ \atAssetClass _redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx') -> unTermCont $ do
      ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
      PTxInfo txInfo' <- pmatchC $ pfromData ctx.txInfo
      txInfo <- pletFieldsC @'["inputs", "mint", "outputs"] txInfo'
      let inputs = txInfo.inputs
          mintedValue = pfromData txInfo.mint
          govTokenSpent = pisTokenSpent # atAssetClass # inputs

      PMinting ownSymbol' <- pmatchC $ pfromData ctx.purpose

      let ownSymbol = pfromData $ pfield @"_0" # ownSymbol'
          mintedATs = passetClassValueOf # mintedValue # (passetClass # ownSymbol # pconstant "")
      pure $
        pif
          (0 #< mintedATs)
          ( unTermCont $ do
              pguardC "Parent token did not move in minting GATs" govTokenSpent
              pguardC "All outputs only emit valid GATs" $
                pall
                  # plam
                    (authorityTokensValidIn # ownSymbol #)
                  # txInfo.outputs
              pure $ popaque $ pconstant ()
          )
          (popaque $ pconstant ())
