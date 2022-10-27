# Revision history for agora

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased (Candidate for 1.0.0)

### Modified

- Fix several vulnerabilities and bugs found in both staking and proposal components.

  Including:

  - Proposal thresholds should be inclusively checked.
  - Attackers can fail any voted-on/locked proposal, or fast track to `Finished`,
    by constructing a transaction that has a very loose valid time range.
  - The stake validator can be fooled by stakes that doesn't belong to itself, and
    consequently allows attack to down vote without voting.
  - Improve doc string of `authorityTokensValidIn` to avoid confusion.
  - Rename proposal redeemer `Unlock` to `UnlockStake` to avoid confusion.

  Included by [#200](https://github.com/Liqwid-Labs/agora/pull/200)

- Fix a bug where `lockedBy` and `delegatedTo` fields of stake datums aren't checked
  during the creation of stakes.

  Included by [#199](https://github.com/Liqwid-Labs/agora/pull/199)

- Fix several vulnerabilities and bugs found in staking components.
  
  Including:

  - Stake state token can be taken away
  - Privilege escalation: Acting on behalf of delegatee role + Unlocking delegated stakes
  - Delegatee can steal delegated inputs
  - Stake policy doesn't allow destroying multiple stakes

  Included by [#195](https://github.com/Liqwid-Labs/agora/pull/195)

- Place a lock the stake while cosigning a proposal.

  NOTE: This changes how cosigning works. In particular, the stake has to be
  spent instead of just presented in the reference inputs. Also, adding multiple
  cosignatures in one tx is no longer possible.

  Included by [#192](https://github.com/Liqwid-Labs/agora/pull/192)

- Support voting/retracting votes with multiple stakes.

  NOTE: Due to the fact that the order of stake locks is undefined, voting to
   multiple proposals in a single tx is disallowed.
  
  Included by [#186](https://github.com/Liqwid-Labs/agora/pull/186)

- Fix a bug that allows an attacker to send two or more GATs to an effect in the winning effect group.

  Fixed by [#181](https://github.com/Liqwid-Labs/agora/pull/181)

- Workaround `currentProposalTime` always returns `PNothing`, due to the fact
 that upper bound of `txInfoValidRange` is never closed.

  Fixed by [#177](https://github.com/Liqwid-Labs/agora/pull/177)

- Fixed governor validator always fail because of the 0 ADA entry in
 `txInfoF.mint`. (#174)

  Fixed by [#175](https://github.com/Liqwid-Labs/agora/pull/175)

- Standalone stake redeemers. This allows injecting custom validation logic to
the stake validator easily. The behaviour of the default stake validator remains
 unchanged.

  Included by [#172](https://github.com/Liqwid-Labs/agora/pull/172)

- Witness stakes with reference input. Stake redeemer `WitnessStake` is removed.

  Included by [#168](https://github.com/Liqwid-Labs/agora/pull/168)

- `tracing` flag in `ScriptParams` of `agora-scripts` to enable/disable tracing in exported scripts.

  NOTE: This changes the representation of `ScriptParams`. In order to preserve old behavior, the flag
  must be set to `True`.
  
  Included by [#167](https://github.com/Liqwid-Labs/agora/pull/167).

- `effects` of `Proposaldatum` is now required to be sorted in ascending order. The uniqueness of result tags is also guaranteed.

  `ProposalVotes` should be sorted the same way as a result.

- AuthCheck script is used for tagging GAT TokenName instead of effect script
  it is deployed at.
  
  Included by [#161](https://github.com/Liqwid-Labs/agora/pull/161).

- Use `Credential` instead of `PubKeyHash`

  Included by [#158](https://github.com/Liqwid-Labs/agora/pull/158).

  NOTE: This changes the representation of the following types:
  
  - `PStakeDatum`
  - `PStakeRedeemer`
  - `PProposalDatum`
  - `PProposalRedeemer`

- Use plutus v2 types.

  Included by [#156](https://github.com/Liqwid-Labs/agora/pull/156).

## 0.2.0 -- 2022-08-13

### Added

- Script exporting with `plutarch-script-export`.

### Modified

- Bump plutarch to 1.2 and use `liqwid-nix` for flake derivation.

  Included by [#150](https://github.com/Liqwid-Labs/agora/pull/150).

- Script building uses the lazy record `AgoraScripts` instead of explicit per-component parameters.

  Included by [#150](https://github.com/Liqwid-Labs/agora/pull/150).

- Stake delegation.
  
  Included by [#149](https://github.com/Liqwid-Labs/agora/pull/149).

- Fixed bug that checks the proposal thresholds in an incorrect way. Added negative tests for the governor scripts.

  Included by [#146](https://github.com/Liqwid-Labs/agora/pull/146).

- Draft phase and cosigning for Proposals. 

  Included by [#136](https://github.com/Liqwid-Labs/agora/pull/136).

- Fixed bug with regards to moving from `VotingReady`.

  Included by [#134](https://github.com/Liqwid-Labs/agora/pull/134).
  
- Fixed bug that made it impossible to create proposals. Added new stake locking mechanism for creating proposals. 
  
  Included by [#142](https://github.com/Liqwid-Labs/agora/pull/142).
  
  NOTE: This changes the representation of the following types:
  
  - `PProposalLock`
  - `PStakeDatum`
  - `PStakeRedeemer`
  - `PProposalRedeemer`
  - `PTreasuryRedeemer`
  - `PGovernorDatum`
  
### Removed

- Side-stream utilies into `liqwid-Labs/liqwid-plutarch-extra`
  - `Agora.MultiSig`--entire module.
  - `scriptHashFromAddress` to `Plutarch.Api.V1.ScriptContext`.
  - `findOutputsToAddress` to `Plutarch.Api.V1.ScriptContext`.
  - `findTxOutDatum` to `Plutarch.Api.V1.ScriptContext`.
  - `hasOnlyOneTokenOfCurrencySymbol` to `Plutarch.Api.V1.Value`.
  - `mustBePJust` to `Plutarch.Extra.Maybe`.
  - `mustBePDJust` to `Plutarch.Extra.Maybe`.
  - `isScriptAddress` to `Plutarch.Api.V1.ScriptContext`.
  - `isPubKey` to `Plutarch.Api.V1.ScriptContext`.
  - `pisUniqBy'` to `Plutarch.Extra.List`.
  - `pisUniq'` to `Plutarch.Extra.List`.
  - `pon` to `Plutarch.Extra.Function`.
  - `pbuiltinUncurry` to `Plutarch.Extra.Function`.

## 0.1.0 -- 2022-06-22

### Added

* First release
