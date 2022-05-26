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

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
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
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.AssetClass (passetClass, passetClassValueOf)
import "plutarch" Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Builtin (pforgetData)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC

--------------------------------------------------------------------------------

import Agora.Utils (
  allOutputs,
  plookup,
  psymbolValueOf,
  ptokenSpent,
  tcassert,
  tcmatch,
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
  deriving stock (GHC.Generic)

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
  plam $ \authorityTokenSym txOut'' -> unTermCont $ do
    PTxOut txOut' <- tcmatch txOut''
    txOut <- tcont $ pletFields @'["address", "value"] $ txOut'
    PAddress address <- tcmatch txOut.address
    PValue value' <- tcmatch txOut.value
    PMap value <- tcmatch value'
    pure $
      pmatch (plookup # pdata authorityTokenSym # value) $ \case
        PJust (pfromData -> tokenMap') ->
          pmatch (pfield @"credential" # address) $ \case
            PPubKeyCredential _ ->
              -- GATs should only be sent to Effect validators
              ptraceIfFalse "authorityTokensValidIn: GAT incorrectly lives at PubKey" $ pconstant False
            PScriptCredential ((pfromData . (pfield @"_0" #)) -> cred) -> unTermCont $ do
              PMap tokenMap <- tcmatch tokenMap'
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

-- | Assert that a single authority token has been burned.
singleAuthorityTokenBurned ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s (PAsData PTxInfo) ->
  Term s PValue ->
  Term s PBool
singleAuthorityTokenBurned gatCs txInfo mint = unTermCont $ do
  let gatAmountMinted :: Term _ PInteger
      gatAmountMinted = psymbolValueOf # gatCs # mint

  txInfoF <- tcont $ pletFields @'["inputs"] $ txInfo

  pure $
    foldr1
      (#&&)
      [ ptraceIfFalse "singleAuthorityTokenBurned: Must burn exactly 1 GAT" $ gatAmountMinted #== -1
      , ptraceIfFalse "singleAuthorityTokenBurned: All GAT tokens must be valid at the inputs" $
          pall
            # plam
              ( \txInInfo' -> unTermCont $ do
                  PTxInInfo txInInfo <- tcmatch (pfromData txInInfo')
                  let txOut' = pfield @"resolved" # txInInfo
                  pure $ authorityTokensValidIn # gatCs # pfromData txOut'
              )
            # txInfoF.inputs
      ]

-- | Policy given 'AuthorityToken' params.
authorityTokenPolicy :: AuthorityToken -> ClosedTerm PMintingPolicy
authorityTokenPolicy params =
  plam $ \_redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx') -> unTermCont $ do
      ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
      PTxInfo txInfo' <- tcmatch $ pfromData ctx.txInfo
      txInfo <- tcont $ pletFields @'["inputs", "mint"] txInfo'
      let inputs = txInfo.inputs
          mintedValue = pfromData txInfo.mint
          AssetClass (govCs, govTn) = params.authority
          govAc = passetClass # pconstant govCs # pconstant govTn
          govTokenSpent = ptokenSpent # govAc # inputs

      PMinting ownSymbol' <- tcmatch $ pfromData ctx.purpose

      let ownSymbol = pfromData $ pfield @"_0" # ownSymbol'
          mintedATs = passetClassValueOf # mintedValue # (passetClass # ownSymbol # pconstant "")
      pure $
        pif
          (0 #< mintedATs)
          ( unTermCont $ do
              tcassert "Parent token did not move in minting GATs" govTokenSpent
              tcassert "All outputs only emit valid GATs" $
                allOutputs @PData # pfromData ctx.txInfo #$ plam $ \txOut _value _address _datum ->
                  authorityTokensValidIn
                    # ownSymbol
                    # txOut
              pure $ popaque $ pconstant ()
          )
          (popaque $ pconstant ())
