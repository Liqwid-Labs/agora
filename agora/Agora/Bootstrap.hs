{- | Module     : Agora.Bootstrap
     Maintainer : connor@mlabs.city
     Description: Initialize a governance system

     Initialize a governance system
-}
module Agora.Bootstrap (agoraScripts, agoraScripts', alwaysSucceedsPolicyRoledScript) where

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
import Plutarch.Api.V2 (PMintingPolicy)
import Plutarch.Extra.Compile (mustCompile)
import Ply (ScriptRole (MintingPolicyRole), TypedScriptEnvelope)
import Ply.Plutarch.TypedWriter (TypedWriter, mkEnvelope)
import ScriptExport.ScriptInfo (RawScriptExport (..), RoledScript (..))

{- | Parameterize core scripts, given the 'Agora.Governor.Governor'
     parameters and plutarch configurations.

     @since 1.0.0
-}
agoraScripts :: Config -> RawScriptExport
agoraScripts conf =
  RawScriptExport $
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
  where
    envelope ::
      forall (pt :: S -> Type).
      TypedWriter pt =>
      Text ->
      ClosedTerm pt ->
      (Text, TypedScriptEnvelope)
    envelope d t = (d, either (error . unpack) id $ mkEnvelope conf d t)

agoraScripts' :: Config -> Either Text [TypedScriptEnvelope]
agoraScripts' conf =
  sequenceA
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
  where
    envelope ::
      forall (pt :: S -> Type).
      TypedWriter pt =>
      Text ->
      ClosedTerm pt ->
      Either Text TypedScriptEnvelope
    envelope = mkEnvelope conf

{- | A minting policy that always succeeds.

  NOTE(Emily, Jan 3rd 2023): Adding this in here because it's useful for testnet GT.
    In reality, it shouldn't be used by anyone on mainnet, but removing it is not
    productive for off-chain testing.

  @since 1.0.0
-}
alwaysSucceedsPolicyRoledScript :: RoledScript
alwaysSucceedsPolicyRoledScript =
  RoledScript
    { script = mustCompile @PMintingPolicy $ plam $ \_ _ -> popaque $ pcon PUnit
    , role = MintingPolicyRole
    }
