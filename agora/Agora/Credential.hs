{- |
Module     : Agora.Stake.Scripts
Maintainer : emi@haskell.fyi
Description: Functions for dealing with generalized credentials.

Functions for dealing with generalized credentials.
-}
module Agora.Credential (PAuthorizationCredential, AuthorizationCredential, pauthorizedBy, authorizationContext) where

import GHC.Records (HasField)
import Plutarch.Api.V1 (PCredential (..), PPubKeyHash)
import Plutarch.Api.V2 (PTxInInfo (..))
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import Plutarch.Extra.TermCont (pmatchC)
import PlutusLedgerApi.V2 (Credential)

{- | This type represents a general purpose authority which can be used
     to check for approval.

     The most basic case is simply a PubKeyCredential.

     @since 1.0.0
-}
type AuthorizationCredential =
  Credential

type PAuthorizationCredential =
  PCredential

{- | Context required in order to check 'AuthorizationCredential'.

     @since 1.0.0
-}
data PAuthorizationContext (s :: S) = PAuthorizationContext
  { signatories :: Term s (PBuiltinList (PAsData PPubKeyHash))
  , inputs :: Term s (PBuiltinList PTxInInfo)
  }
  deriving stock
    ( -- | @since 1.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    , -- | @since 1.0.0
      PEq
    )

-- | @since 1.0.0
instance DerivePlutusType PAuthorizationContext where
  type DPTStrat _ = PlutusTypeScott

{- | Build up 'PAuthorizationContext' from fields.

     @since 1.0.0
-}
authorizationContext ::
  forall {r} {s :: S}.
  ( HasField "inputs" r (Term s (PBuiltinList PTxInInfo))
  , HasField "signatories" r (Term s (PBuiltinList (PAsData PPubKeyHash)))
  ) =>
  r ->
  Term s PAuthorizationContext
authorizationContext f =
  pcon (PAuthorizationContext f.signatories f.inputs)

{- | Check for authorization by credential.

     @since 1.0.0
-}
pauthorizedBy :: forall (s :: S). Term s (PAuthorizationContext :--> PAuthorizationCredential :--> PBool)
pauthorizedBy = phoistAcyclic $
  plam $ \ctx credential -> unTermCont $ do
    ctxF <- pmatchC ctx
    pure $
      pmatch credential $ \case
        PPubKeyCredential ((pfield @"_0" #) -> pk) ->
          ptxSignedBy # ctxF.signatories # pk
        PScriptCredential ((pfield @"_0" #) -> _) ->
          pany
            # plam
              ( \input ->
                  (pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # input)
                    #== credential
              )
            # ctxF.inputs
