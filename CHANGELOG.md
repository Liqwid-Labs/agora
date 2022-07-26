# Revision history for agora

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).


## Unreleased (Candidate for 0.2.0)

### Added

- Script exporting with `plutarch-script-export` 

### Modified

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

## 0.1.0 -- 2022-06-22

### Added

* First release
