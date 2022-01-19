# Proposals Technical Design

This document gives an overview of technical design aspects of the Proposals system for introducing, voting on and executing Governance Proposals.

| Spec Status | Impl Status | Last Revision |
|-------------|-------------|---------------|
| WIP         |  WIP        | 2022-01-19    |

--------------------

**Spec Ownership:** [@Emily Martins]

**Authors**: [@Emily Martins]

**Impl Owner:** [@Emily Martins]

[@Emily Martins]: https://github.com/emiflake

--------------------

## Proposals

Initiating a proposal requires you to have more than a certain amount of LQ in the StakingPool (e.g. around 6-10 LQ). This is checked by consuming the proposer's stake utxo. Creating a proposal forges a proposal script state thread which is where all voting interactions happen with. Proposals are tracked in coordination with the governance, so it must be included in the transaction as well.

### Proposal voting

##### Overview of stages

A proposal fits into a state machine neatly. Draft being the starting state, and Executed or Failed being the final states. It should be noted that, for optimization reasons, not all states are exactly physical, instead, the current state is a product of the current time as well. For instance, after the voting has ended, no state transition on-chain occurs.

![](../diagrams/ProposalStateMachine.svg)

##### When is each interaction valid?

- `S`: When the proposal was created
- `D`: The length of the draft period
- `V`: The length of the voting period
- `L`: The length of the locking period
- `E`: The length of the execution period

| Action                              | Valid POSIXTimeRange               | Valid *stored* state(s) |
|-------------------------------------|------------------------------------|-------------------------|
| Witness                             | [S, ∞)                             | *                       |
| Cosign                              | [S, S + D)                         | Draft                   |
| AdvanceProposal                     | [S, S + D)                         | Draft                   |
| Vote                                | [S + D, S + D + V)                 | Voting                  |
| Unlock                              | [S + D, ∞)                         | *                       |
| CountVotes (? see spec comment)     | [S + D + V, S + D + V + L)         | Voting                  |
| ExecuteProposal (if quorum reached) | [S + D + V + L, S + D + V + L + E) | Voting                  |

```diff
- CountVotes takes a snapshot after voting has ended, it allows users to unlock their stake without affecting votes, after the lock period has ended. This is an optional feature of locking, do we want this?
```

#### Draft phase

During the Draft phase, the proposal script has been minted. At this stage, only positive votes will count and for the proposal to get into the voting phase, a certain amount of LQ will have to be backing the proposal. The UTXO can be queried for any metadata it contains, should it contain any useful information. LQ holders can "cosign" the proposal and add their LQ in order to allow the motion to get into the voting phase.

#### Voting phase

During the voting phase a voter can submit a vote either in favour or against the proposal. This simply adds their PubKey to the list of votes as a state-machine action. By virtue of being a state-machine action, this causes contention, and may be rate-limited through some means.

##### What determines a successful proposal?

When initializing a proposal, the governance's thresholds are passed along, one of the thresholds is the `quorum`.

#### Lock phase

After the voting phase has completed, the proposal has either passed, or failed.

We wait a set time before allowing execution of the proposal's effects. This is to allow LQ holders time to prepare for the change in whichever way they seem fit. This is also a security mechanism, should the system be at risk of hostile takeover.

#### Execution phase

In the case of a failed proposal, the proposal will simply be frozen in time and never interacted with anymore. There is no value locked behind a proposal, besides the state thread itself, so nothing is lost.

In the case of a successful proposal, anyone can execute the effects, as it can only be done in a lawful way.

There is a deadline for how long it can take for the proposal to get executed. Again, as the proposal is necessarily something that has been agreed by the voters, it will be incentivized naturally to be executed.

See [tech-design/authority-token.md](./authority-tokens.md) for how effects take place after execution
