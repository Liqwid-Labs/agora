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

Initiating a proposal requires the initiator to have more than a certain amount of LQ in the StakingPool (e.g. around 6-10 LQ). This is checked by consuming the proposer's stake utxo. Creating a proposal forges a proposal script state thread which is where all voting interactions happen within. Proposals are tracked in coordination with the governance, so it must be included in the transaction as well.

> Peter, 2022-01-24: "(...) than a certain amount of **LQ**". two questions: 1.) Is this up to date? 2.) Given that agora doesn't have a token, what should this token be called in the abstract?
> Peter, 2022-01-24: "(...) coordination with the governance": We've adopted the terminology in LiqwidX "Governor" to refer to the continuing UTxO that tracks governance-related activities, and use "governance" to refer to "what the Governor does/enables". Do you make the same distinction?

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

During the Draft phase, the proposal script has been minted. At this stage, only votes in favor of cosigning the draft will count. For the proposal to get into the voting phase, a certain amount of LQ will have to be backing the proposal. The UTXO can be queried for any metadata it contains, should it contain any useful information. LQ holders can "cosign" the proposal and add their LQ in order to allow the motion to get into the voting phase.

> Peter, 2022-01-24: "(...) the proposal script has been minted." I think this should be "the draft proposal's state thread token has been minted and paid to the proposal's UTxO."
> Peter, 2022-01-24: "(...) a certain amount of LQ": how much? Where is this set/calculated?

#### Voting phase

During the voting phase a voter can submit a vote either in favour or against the proposal. This simply adds their PubKey to the list of votes as a state-machine action. By virtue of being a state-machine action, this causes contention, and may be rate-limited through some means.

> Peter, 2022-01-24: What happens in the event of a tie? How are the votes tailled? Is it always simple majority, or are more complex [social choice procedures](https://en.wikipedia.org/wiki/Social_choice_theory) in-scope for Agora (even if its V2, V3...)

##### What determines a successful proposal?

When initializing a proposal, the governance's thresholds are passed along, one of the thresholds is the `quorum`.

#### Lock phase

After the voting phase has completed, the proposal has either passed, or failed.

We wait a set time before allowing execution of the proposal's effects. This is to allow LQ holders time to prepare for the change in whichever way they see fit. This is also a security mechanism, should the system be at risk of hostile takeover.

> Peter, 2022-01-24: How is this time decided? Are you familiar with the "Emergency Shutdown Mode" of MakerDAO, which we'll be adopting in LiqwidX? This is something that would, for the most part, need to happen ASAP. What sort of modularity exists or would you like to see?

#### Execution phase

In the case of a failed proposal, the proposal will simply be frozen in time and never interacted with anymore. There is no value locked behind a proposal, besides the state thread itself, so nothing is lost.

In the case of a successful proposal, anyone can execute the effects, as it can only be done in a lawful way.

There is a deadline for how long it can take for the proposal to get executed. Again, as the proposal is necessarily something that has been agreed by the voters, it will be incentivized naturally to be executed.

See [tech-design/authority-token.md](./authority-tokens.md) for how effects take place after execution

> Peter 2022-01-24: I _think_ it may be useful to have some way to witness on-chain whether a proposal succeeded or failed, and by what margins. I'm not certain whether this would be possible with what you currently have in mind, but it could allow for a more expressive governenace structure or allow for other actions to depend on the success/failure of proposals that aren't directly part of the system being governed (i.e., a validator that only locks/unlocks value if a proposal goes a certain way). 
