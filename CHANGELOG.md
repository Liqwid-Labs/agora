# Revision history for agora

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased (Candidate for 1.0.0)

### Modified

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
