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

import Agora.Governor (PGovernorRedeemer (PMintGATs), presolveGovernorRedeemer)
import Agora.SafeMoney (AuthorityTokenTag, GovernorSTTag)
import Agora.Utils (psymbolValueOfT, ptag, ptoScottEncodingT, puntag)
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
  PScriptPurpose (PMinting),
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
 )
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Extra.Bool (passert)
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)
import Plutarch.Extra.Maybe (passertPJust, pfromJust)
import Plutarch.Extra.Sum (PSum (PSum))
import Plutarch.Extra.Tagged (PTagged)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
 )
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Extra.Value (psymbolValueOf')

--------------------------------------------------------------------------------

{- | Check that all GATs are valid in a particular TxOut.

     WARNING: As of version 1.0.0, this has been weakened in order to be
     compatible with RATs. The token name is no loger checked, meaning that a
     GAT can escape from its effect script, if the effect script is vulnerable.
     In order to prevent this, all effect scripts should be implemented carefully,
     and ideally use the trusted effect base. See also 'Agora.Effect'.

     (before 1.0.0) How this is checked: an AuthorityToken should never leave
     the Effect it was initially sent to, so we simply check that
     the script address the token resides in matches the TokenName.
     Since the TokenName was tagged upon mint with the Effect script
     it was sent to, this is enough to prove validity.
     In other words, check that all assets of a particular currency symbol
     are tagged with a TokenName that matches where they live.

     @since 1.0.0
-}
authorityTokensValidIn :: forall (s :: S). Term s (PTagged AuthorityTokenTag PCurrencySymbol :--> PTxOut :--> PBool)
authorityTokensValidIn = phoistAcyclic $
  plam $ \authorityTokenSym txOut'' -> unTermCont $ do
    PTxOut txOut' <- pmatchC txOut''
    txOut <- pletFieldsC @'["address", "value"] $ txOut'
    PAddress address <- pmatchC txOut.address
    PValue value' <- pmatchC txOut.value
    PMap value <- pmatchC value'
    pure $
      pmatch (plookupAssoc # pfstBuiltin # psndBuiltin # pdata (puntag authorityTokenSym) # value) $ \case
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
  Term s (PTagged AuthorityTokenTag PCurrencySymbol) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PValue keys amounts) ->
  Term s PBool
singleAuthorityTokenBurned gatCs inputs mint = unTermCont $ do
  let gatAmountMinted :: Term _ PInteger
      gatAmountMinted = psymbolValueOfT # gatCs # mint

  let inputsWithGAT =
        pfoldMap
          # plam
            ( flip pmatch $ \case
                PTxInInfo txInInfo -> unTermCont $ do
                  resolved <- pletC $ pfield @"resolved" # txInInfo

                  pguardC "While counting GATs at inputs: all GATs must be valid"
                    $ authorityTokensValidIn
                      # gatCs
                      #$ pfromData
                    $ resolved

                  pure . pcon . PSum $
                    psymbolValueOfT
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
authorityTokenPolicy :: ClosedTerm (PTagged GovernorSTTag PAssetClassData :--> PMintingPolicy)
authorityTokenPolicy =
  plam $ \gstAssetClass _redeemer ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfoF <-
      pletFieldsC
        @'[ "inputs"
          , "mint"
          , "outputs"
          , "redeemers"
          ]
        ctxF.txInfo

    PMinting ownSymbol' <- pmatchC $ pfromData ctxF.purpose

    let ownSymbol = pfromData $ pfield @"_0" # ownSymbol'

    PPair mintedATs burntATs <-
      pmatchC $ pfromJust #$ psymbolValueOf' # ownSymbol # txInfoF.mint

    pure $
      popaque $
        pif
          (0 #< mintedATs)
          ( unTermCont $ do
              pguardC "No GAT burnt" $ 0 #== burntATs
              let governorRedeemer =
                    passertPJust
                      # "GST should move"
                      #$ presolveGovernorRedeemer
                      # (ptoScottEncodingT # gstAssetClass)
                      # pfromData txInfoF.inputs
                      # txInfoF.redeemers
              pguardC "Governor redeemr correct" $
                pcon PMintGATs #== governorRedeemer
              pguardC "All outputs only emit valid GATs" $
                pall
                  # plam
                    (authorityTokensValidIn # ptag ownSymbol #)
                  # txInfoF.outputs
              pure $ pconstant ()
          )
          (passert "No GAT minted" (0 #== mintedATs) (pconstant ()))
