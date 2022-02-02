# Proposals technical design

This document gives an overview of the technical design of the proposals system for introducing, voting on and executing governance proposals.

| Specification | Implementation | Last revision |
|:-----------:|:-----------:|:-------------:|
| WIP         |  WIP        | v0.1 2022-02-02    |

***

**Specification ownership:** [Emily Martins]

**Authors**:

-   [Emily Martins]
-   [Jack Hodgkinson]

**Implementation ownership:** [Emily Martins]

[Emily Martins]: https://github.com/emiflake

[Jack Hodgkinson]: https://github.com/jhodgdev

**Current status**:

Imported from Liqwid by [Emily Martins]. Underwent rewrite by [Jack Hodgkinson].Further amendments to the 'period table' should be considered. Outstanding remarks from Peter Dragos that require attention.

***

## Proposals

Initiating a proposal requires the proposer to have more than a certain amount of GT staked in the system e.g. 5GT. This is checked by consuming the UTXO representing the user's stake. Initiating a proposal creates a script, which will handle all voting interactions.

## Voting

### Voting stages

The life-cycle of a proposal is neatly represented by a state machine, with the 'draft' phase being the initial state, and 'executed' and 'failed' being the terminating states. Please note that this state-machine representation is purely conceptual and should not be expected to reflect technical implementation.

![](../diagrams/ProposalStateMachine.svg)

#### When may interactions occur?

Consider the following 'stages' of a proposal:

-   `S`: when the proposal was created.
-   `D`: the length of the draft period.
-   `V`: the length of the voting period.
-   `L`: the length of the locking period.
-   `E`: the length of the execution period.

| Action                              | Valid POSIXTimeRange               | Valid _stored_ state(s) |
|-------------------------------------|------------------------------------|-------------------------|
| Witness                             | \[S, ∞]                             | \*                       |
| Cosign                              | \[S, S + D]                         | Draft                   |
| AdvanceProposal                     | \[S, S + D]                         | Draft                   |
| Vote                                | \[S + D, S + D + V]                 | Voting                  |
| Unlock                              | \[S + D, ∞]                         | \*                       |
| CountVotes (? see spec comment)     | \[S + D + V, S + D + V + L]         | Voting                  |
| ExecuteProposal (if quorum reached) | \[S + D + V + L, S + D + V + L + E] | Voting                  |

```diff
- CountVotes takes a snapshot after voting has ended, it allows users to unlock their stake without affecting votes, after the lock period has ended. This is an optional feature of locking, do we want this?
```

> Jack 2022-02-02: I will consider revising this table further at a later time.

#### Draft phase

During the draft phase, the proposal script has been minted. At this stage, only votes in favor of co-signing the draft are counted. For the proposal to transition to the voting phase, a threshold of GT will have to be staked backing the proposal. This threshold will be determined on a per-system basis and could itself be a 'governable' parameter.

#### Voting phase

In the voting phase, users may utilise their stakes to vote in-favour or in-opposition to a proposal. This adds their public key to the proposal. There is potential for contention within the system and therefore may have to be rate-limited. The method by which a user's stakes are weighted and the thresholds required for proposals to pass are determined on a per-protocol basis.

#### Lock phase

Upon completion of the voting phase, a proposal will either have been passed or failed. A delay between the passing of a proposal and execution of its effects will be enforced, to allow users to prepare for incoming changes to the system. It'll further give the system maintainers opportunity to intervene, in the case of a hostile action.

> Peter, 2022-01-24: How is this time decided? Are you familiar with the "Emergency Shutdown Mode" of MakerDAO, which we'll be adopting in LiqwidX? This is something that would, for the most part, need to happen ASAP. What sort of modularity exists or would you like to see?

#### Execution phase

Failed proposals will not be interacted with further. The only value they will contain is their state thread tokens, so nothing of worth is lost.

Successful proposals will be verified by the governor component, which will issue ['governance authority tokens'](/docs/tech-design/authority-tokens.md) (GATs) to a proposal's separate 'effects'. The burning of these tokens will be a pre-requisite for system changes to be made, therefore the possession of one will serve as a form of 'licence' for making the changes.

There will be a deadline before which a proposal's effects will have to be executed. As any passed proposal's effects will necessarily have been supported by the community, it can be presumed that community members will have be incentivised to ensure the effects are enacted soon after the proposal has been passed.
