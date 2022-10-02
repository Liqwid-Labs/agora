{- | Module     : Agora.Bootstrap
     Maintainer : connor@mlabs.city
     Description: Initialize a governance system

     Initialize a governance system
-}
module Agora.Bootstrap (agoraScripts, writeAgoraScripts) where

import Agora.AuthorityToken (authorityTokenPolicy)
import Agora.Effect.GovernorMutation (mutateGovernorValidator)
import Agora.Effect.NoOp (noOpValidator)
import Agora.Effect.TreasuryWithdrawal (treasuryWithdrawalValidator)
import Agora.Governor.Scripts (governorPolicy, governorValidator)
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Control.Monad (void)
import Data.Map (Map, fromList, traverseWithKey)
import Data.Text (Text, unpack)
import Plutarch (Config)
import Plutarch.Extra.AssetClass (PAssetClass)
import PlutusLedgerApi.V1.Value (AssetClass)
import Ply (TypedScriptEnvelope)
import Ply.Core.Serialize (writeEnvelope)
import Ply.Plutarch.Class (PlyArgOf)
import Ply.Plutarch.TypedWriter (TypedWriter, mkEnvelope)
import System.FilePath ((</>))

type instance PlyArgOf PAssetClass = AssetClass

{- | Parameterize core scripts, given the 'Agora.Governor.Governor'
     parameters and plutarch configurations.

     @since 0.2.0
-}
agoraScripts :: Config -> Map Text TypedScriptEnvelope
agoraScripts conf =
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

{- | Write all agora scripts to the given path.

     @since 0.2.0
-}
writeAgoraScripts :: Config -> FilePath -> IO ()
writeAgoraScripts conf path =
  void $
    traverseWithKey
      (\name ts -> writeEnvelope (path </> unpack name <> ".plutus") ts)
      $ agoraScripts conf
