module Agora.AuthorityToken (
  authorityTokenPolicy,
  AuthorityToken (..),
) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------
import Plutus.V1.Ledger.Value (AssetClass (..))

--------------------------------------------------------------------------------

import Plutarch.Api.V1
import Plutarch.List (pfoldr')
import Plutarch.Prelude

--------------------------------------------------------------------------------

{- | An AuthorityToken represents a proof that a particular token
     moved while this token was minted. In effect, this means that
     the validator that locked such a token must have approved
     said transaction. Said validator should be made aware of
     _this_ token's existence in order to prevent incorrect minting.
-}
newtype AuthorityToken = AuthorityToken
  { -- | Token that must move in order for minting this to be valid.
    authority :: AssetClass
  }

--------------------------------------------------------------------------------

-- TODO: upstream something like this
pfind' ::
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p =
  precList
    (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
    (const $ pcon PNothing)

-- TODO: upstream something like this
plookup ::
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndBuiltin # p))

passetClassValueOf' :: AssetClass -> Term s (PValue :--> PInteger)
passetClassValueOf' (AssetClass (sym, token)) =
  passetClassValueOf # pconstant sym # pconstant token

passetClassValueOf ::
  Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PInteger)
passetClassValueOf =
  phoistAcyclic $
    plam $ \sym token value'' ->
      pmatch value'' $ \(PValue value') ->
        pmatch value' $ \(PMap value) ->
          pmatch (plookup # pdata sym # value) $ \case
            PNothing -> 0
            PJust m' ->
              pmatch (pfromData m') $ \(PMap m) ->
                pmatch (plookup # pdata token # m) $ \case
                  PNothing -> 0
                  PJust v -> pfromData v

authorityTokenPolicy ::
  AuthorityToken ->
  Term s (PData :--> PData :--> PScriptContext :--> PUnit)
authorityTokenPolicy params =
  plam $ \_datum _redeemer ctx' ->
    pmatch ctx' $ \(PScriptContext ctx) ->
      let txInfo' = pfromData $ pfield @"txInfo" # ctx
          purpose' = pfromData $ pfield @"purpose" # ctx

          inputs =
            pmatch txInfo' $ \(PTxInfo txInfo) ->
              pfromData $ pfield @"inputs" # txInfo

          authorityTokenInputs =
            pfoldr'
              ( \txInInfo' acc ->
                  pmatch (pfromData txInInfo') $ \(PTxInInfo txInInfo) ->
                    let txOut' =
                          pfromData $ pfield @"resolved" # txInInfo
                        txOutValue =
                          pmatch txOut' $
                            \(PTxOut txOut) ->
                              pfromData $ pfield @"value" # txOut
                     in passetClassValueOf' params.authority # txOutValue + acc
              )
              # (0 :: Term s PInteger)
              # inputs

          -- We incur the cost twice here. This will be fixed upstream in Plutarch.
          mintedValue =
            pmatch txInfo' $ \(PTxInfo txInfo) ->
              pfromData $ pfield @"mint" # txInfo

          tokenMoved = 0 #< authorityTokenInputs
       in pmatch purpose' $ \case
            PMinting sym' ->
              let sym = pfromData $ pfield @"_0" # sym'
                  mintedATs = passetClassValueOf # sym # pconstant "" # mintedValue
               in pif
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
            _ ->
              ptraceError "Wrong script type"
