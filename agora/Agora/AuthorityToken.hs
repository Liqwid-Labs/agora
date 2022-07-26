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

import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (
  AmountGuarantees,
  KeyGuarantees,
  PAddress (..),
  PCredential (..),
  PCurrencySymbol (..),
  PMintingPolicy,
  PScriptContext (..),
  PScriptPurpose (..),
  PTxInInfo (PTxInInfo),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Api.V1.AssetClass (passetClass, passetClassValueOf)
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.ScriptContext (pisTokenSpent)
import "liqwid-plutarch-extra" Plutarch.Api.V1.Value (psymbolValueOf)
import "plutarch" Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.List (plookup)
import Plutarch.Extra.TermCont (pguardC, pletFieldsC, pmatchC)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))

--------------------------------------------------------------------------------

{- | An AuthorityToken represents a proof that a particular token
     spent in the same transaction the AuthorityToken was minted.
     In effect, this means that the validator that locked such a token
     must have approved the transaction in which an AuthorityToken is minted.
     Said validator should be made aware of an AuthorityToken token's existence
     in order to prevent incorrect minting.

     @since 0.1.0
-}
newtype AuthorityToken = AuthorityToken
  { authority :: AssetClass
  -- ^ Token that must move in order for minting this to be valid.
  }
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )

--------------------------------------------------------------------------------

{- | Check that all GATs are valid in a particular TxOut.
     How this is checked: an AuthorityToken should never leave
     the Effect it was initially sent to, so we simply check that
     the script address the token resides in matches the TokenName.
     Since the TokenName was tagged upon mint with the Effect script
     it was sent to, this is enough to prove validity.
     In other words, check that all assets of a particular currency symbol
     are tagged with a TokenName that matches where they live.

     @since 0.1.0
-}
authorityTokensValidIn :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
authorityTokensValidIn = phoistAcyclic $
  plam $ \authorityTokenSym txOut'' -> unTermCont $ do
    PTxOut txOut' <- pmatchC txOut''
    txOut <- pletFieldsC @'["address", "value"] $ txOut'
    PAddress address <- pmatchC txOut.address
    PValue value' <- pmatchC txOut.value
    PMap value <- pmatchC value'
    pure $
      pmatch (plookup # pdata authorityTokenSym # value) $ \case
        PJust (pfromData -> tokenMap') ->
          pmatch (pfield @"credential" # address) $ \case
            PPubKeyCredential _ ->
              -- GATs should only be sent to Effect validators
              ptraceIfFalse "authorityTokensValidIn: GAT incorrectly lives at PubKey" $ pconstant False
            PScriptCredential ((pfromData . (pfield @"_0" #)) -> cred) -> unTermCont $ do
              PMap tokenMap <- pmatchC tokenMap'
              pure $
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

{- | Assert that a single authority token has been burned.

     @since 0.2.0
-}
singleAuthorityTokenBurned ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PValue keys amounts) ->
  Term s PBool
singleAuthorityTokenBurned gatCs inputs mint = unTermCont $ do
  let gatAmountMinted :: Term _ PInteger
      gatAmountMinted = psymbolValueOf # gatCs # mint

  pure $
    foldr1
      (#&&)
      [ ptraceIfFalse "singleAuthorityTokenBurned: Must burn exactly 1 GAT" $ gatAmountMinted #== -1
      , ptraceIfFalse "singleAuthorityTokenBurned: All GAT tokens must be valid at the inputs" $
          pall
            # plam
              ( \txInInfo' -> unTermCont $ do
                  PTxInInfo txInInfo <- pmatchC (pfromData txInInfo')
                  let txOut' = pfield @"resolved" # txInInfo
                  pure $ authorityTokensValidIn # gatCs # pfromData txOut'
              )
            # inputs
      ]

{- | Policy given 'AuthorityToken' params.

     @since 0.1.0
-}
authorityTokenPolicy :: AuthorityToken -> ClosedTerm PMintingPolicy
authorityTokenPolicy params =
  plam $ \_redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx') -> unTermCont $ do
      ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
      PTxInfo txInfo' <- pmatchC $ pfromData ctx.txInfo
      txInfo <- pletFieldsC @'["inputs", "mint", "outputs"] txInfo'
      let inputs = txInfo.inputs
          mintedValue = pfromData txInfo.mint
          AssetClass (govCs, govTn) = params.authority
          govAc = passetClass # pconstant govCs # pconstant govTn
          govTokenSpent = pisTokenSpent # govAc # inputs

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
                    ( (authorityTokensValidIn # ownSymbol #)
                        . pfromData
                    )
                  # txInfo.outputs
              pure $ popaque $ pconstant ()
          )
          (popaque $ pconstant ())
