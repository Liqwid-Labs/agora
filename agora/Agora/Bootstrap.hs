{- | Module     : Agora.Bootstrap
     Maintainer : connor@mlabs.city
     Description: Initialize a governance system

     Initialize a governance system
-}
module Agora.Bootstrap (agoraScripts) where

import Agora.AuthorityToken (authorityTokenPolicy)
import Agora.Effect.GovernorMutation (mutateGovernorValidator)
import Agora.Effect.NoOp (noOpValidator)
import Agora.Effect.TreasuryWithdrawal (treasuryWithdrawalValidator)
import Agora.Governor.Scripts (governorPolicy, governorValidator)
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Data.Map (fromList)
import Data.Text (Text, unpack)
import Plutarch (Config)
import Plutarch.Extra.AssetClass (PAssetClass)
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply (TypedScriptEnvelope)
import Ply.Plutarch.Class (PlyArgOf)
import Ply.Plutarch.TypedWriter (TypedWriter, mkEnvelope)
import ScriptExport.ScriptInfo (RawScriptExport (..))

type instance PlyArgOf PAssetClass = AssetClass

{- | Parameterize core scripts, given the 'Agora.Governor.Governor'
     parameters and plutarch configurations.

     @since 1.0.0
-}
agoraScripts :: Config -> RawScriptExport
agoraScripts conf =
  RawScriptExport
    { version = "1.0.0"
    , scripts =
        fromList
          [ envelope "agora:governorPolicy" governorPolicy
          , envelope "agora:governorValidator" governorValidator
          , envelope "agora:stakePolicy" stakePolicy
          , envelope "agora:stakeValidator" stakeValidator
          , envelope "agora:proposalPolicy" proposalPolicy
          , envelope "agora:proposalValidator" proposalValidator
          , envelope "agora:treasuryValidator" treasuryValidator
          , envelope "agora:authorityTokenPolicy" authorityTokenPolicy
          , envelope "agora:noOpValidator" noOpValidator
          , envelope "agora:treasuryWithdrawalValidator" treasuryWithdrawalValidator
          , envelope "agora:mutateGovernorValidator" mutateGovernorValidator
          ]
    }
  where
    envelope ::
      forall (pt :: S -> Type).
      TypedWriter pt =>
      Text ->
      ClosedTerm pt ->
      (Text, TypedScriptEnvelope)
    envelope d t = (d, either (error . unpack) id $ mkEnvelope conf d t)
