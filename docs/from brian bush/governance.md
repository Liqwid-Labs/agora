# Governance System Technical Design


| Spec Status | Impl Status | Last Revision |
|-------------|-------------|---------------|
| Superseded by Agora    |  Superseded by Agora        | 2022-01-25    |

--------------------

**Spec Ownership:** N/A

**Authors**: [@Tilde Rose] [@B W Bush]

**Impl Owner:** N/A

**Current Status**:

This spec was written by Tilde/Brian Bush for the LiqwidX project in 2021, but was left unfinished. 
The current efforts in this direction have moved into Agora. 
Nothing in this document should be taken as authoritative or current; this document is being temporarily
preserved in case some of the material within is useful in Agora.

[@Tilde Rose]: https://github.com/t1lde

[@B W Bush]: https://github.com/bwbush

--------------------


```diff
- The bold+italic text below indicates content that might be controversial or
- that attempts to unilaterally resolve or reconcile ambiguities and conflicts
- in the source documents.
```


## Summary

The LiqwidX governance system comprises voting and staking. Those who stake `LQx` ("stakers") may vote on [proposals](proposals.md) for altering governance parameters or the parameters for the `LQUSD` market. Stakers may delegate their stake to other LiqwidX participants. They also reap a portion of collected fees as [rewards](rewards.md).


# The Governor

The governor is a state-machine which manages the creation and execution of proposals. It contains it its state a list of ongoing proposals and the current rules for proposing and voting. These deterministically determine the state thread of the proposal scripts. Each proposal is associated with a unique state machine: in particular, this state machine tracks the progress of voting on the proposal.

The governor also references the state for [markets](markets.md), [liquidation pools](liquidation-pool.md), and [oracles](oracles.md). Proposals for governance actions may change the parameters or validators for those aspects of LiqwidX.


## Multisig Custodians for Governance

Some governance actions may require manual intervention by stakeholder-approved custodians who are identified by their public-key hashes. The [specification for proposal execution](proposals.md) provides details how, in a multi-signature context, such custodians can perform system upgrades or take emergency action. These public-key hashes form part of the state of the governor.

```diff
- See https://github.com/mlabs-haskell/liqwidx-specs/pull/2#issuecomment-959764967
- for the initial discussion of this concept. Note that the existence of such
- custodians introduces centralization that may trigger regulator intervention
- (such as by the Securities and Exchange Commission in the USA).
```


## Governance Parameters

All of the governance parameters relate to the proposal-approval process, which is described [below](#voting-on-proposals).

| Parameter                                                   | Unit of Measure                     |
|-------------------------------------------------------------|-------------------------------------|
| duration of draft phase                                     | `POSIXTimeDifference`               |
| duration of review phase                                    | `POSIXTimeDifference`               |
| duration of voting phase                                    | `POSIXTimeDifference`               |
| duration of lock phase                                      | `POSIXTimeDifference`               |
| duration of execution phase                                 | `POSIXTimeDifference`               |
| Threshold (minimum) value for transition to draft phase     | ***Fraction of circulating `LQx`*** |
| Threshold (minimum) value for transition to voting phase    | ***Fraction of circulating `LQx`*** |
| Threshold (minimum) value for transition to execution phase | ***Fraction of circulating `LQx`*** |

```diff
- A review period between draft and voting, where "the proposal can't be
- interacted with at all" is mentioned in
- https://github.com/mlabs-haskell/liqwidx-specs/pull/2#discussion_r740269689.
- Is a review phase really necessary, since voters can review the proposal
- during the voting period? Presumably, the voting phase would be long enough
- for voters to discuss the proposal, make and reverse their votes, etc.
```

```diff
- Ideally, thresholds would be measured in fraction of circulating LQx but
- this may not be feasible because of expense related to computing this from
- global state, in which case the unit of measure for thresholds should be
- quantity of LQx.
```


# Voting on Proposals


## Life Cycle of A Proposal

Proposals move from initiation through *draft*, *review*, *voting*, *locking*, and *execution* phases. Each phase has constraints on duration (measured in differences of `POSIXTime`) and on the quantity of staked `LQx` required to move to the next phase. Proposals that fail to meet the duration and `LQx` quantity constraints enter a *failed* state, whereas proposals that successfully exit the execution phase reach the *executed* state. The figure below illustrates the timeline for a proposal.

![State machine for proposals.](../images/governance/ProposalStateMachine.svg)

```diff
- Maybe we should use a different term for the "locking" phase? The term "lock"
- is already used in other contexts, so using it here might cause confusion.
```


### Transitions between Phases

In order for a proposal to successfully transition to a subsequent active phase, at least a specified quantity of staked `LQx` must be *cosigned* (in the case of a draft proposal) or *locked* (in the case of a full proposal) by stakers. The governor's state contains phase-specific parameters defining the minimum `LQx` required to successfully transition to the draft, review, voting, locked, and execution phases. That transition is based on the *net* quantity of `LQx` for the proposal at the completion of the last `POSIXTime` of the valid `POSIXTime` range for the phase: i.e., totals earlier in the phase do not affect the proposal's transition. In the voting phase, `LQx` may be locked either in favor or against a proposal, but in other phases it may only be cosigned for a proposal and cosigning against are ignored in the tally: thus, transition into the draft or voting phases is based on the total `LQx` in favor of the proposal, but transition into the execution phase is based on the excess of `LQx` in favor over the `LQx` against.

Staked `LQx` may be freely locked or unlocked at any time***, except in the locking phase***. ***Once the proposal enters a terminal state (i.e., "executed" or "failed"), the stake associated with it is automatically unlocked.*** Staked `LQx` may only be cosigned or locked in a maximum of one proposal at a time, but the `LQx` in any given stake may be apportioned among several proposals: *i.e., the sum of a staker's cosigned and locked stake must be less than or equal to their total stake*. Furthermore, `LQx` that is cosigned or locked for proposals still counts as being staked, so far as rewards computations are concerned.

```diff
- It makes sense to let stakers reverse their votes in the draft, review, and
- voting phases, since this gives them flexibility in responding to new
- information about the proposal's desireability. During the locking phase,
- should they be allowed to remove their lock of LQx they used to vote?
- Similarly for the execution phase. Unlocking during the locking and/or
- execution phases would free that LQx for use in cosigning or voting for other
- proposals that are in earlier phases. That might be okay, but it reduces the
- amount of "skin in the game".
```

```diff
- Automatically unlocking the cosigned or locked LQx would be more complex and
- more expensive for the execution transaction(s) than just taking a "lazy"
- approach and leaving it to the individual stakers to un-cosign or unlock
- the LQx that they had cosigned or locked for a completed proposal. In this
- later case, the stakers would incur the transaction fee for performing this
- operation.
```


### Time Constraints

Let `T₀` be the `POSIXTime` when the proposal was initiated. The governor's state contains parameters `D`, `R`, `V`, `L`, and `E`, which specify the duration (in `POSIXTimeDifference`) of the drafting, review, voting, locking, and execution phases, respectively. Except for the draft and execution phases, the timeline below is inflexible in that a proposal does not advance faster if it meets the advancement criteria early during the valid range of `POSIXTime`. The draft phase may be shorter than `D` if sufficient `LQx` is cosigned to it before `T₀+D`; similarly, the execution phase may be shorter than `E` if the proposal completes execution before `Tₑ+E`.

| Phase     | Activity                          | Duration in `POSIXTime` | Valid Range of `POSIXTime`     |
|-----------|-----------------------------------|-------------------------|--------------------------------|
| Draft     | Cosign proposal (on chain)        | `≤ D`                   | `[T₀, Tᵣ)` where `Tᵣ ≤ T₀ + D` |
| Review    | Study proposal  (off chain)       | `R`                     | `[Tᵣ, Tᵥ)` where `Tᵥ = Tᵣ + R` |
| Voting    | Cast vote (on chain)              | `V`                     | `[Tᵥ, Tₗ)` where `Tₗ = Tᵥ + V` |
| Locking   | Prepare for execution (off chain) | `L`                     | `[Tₗ, Tₑ)` where `Tₑ = Tₗ + L` |
| Execution | Execute proposal (on chain)       | `≤ E`                   | `[Tₑ, T₁)` where `T₁ ≤ Tₑ + E` |

![Life cycle for proposals](governance/ProposalTimeline.svg)
```diff
- N.B.: The SVG file is editable in Inkscape, which was used to draw it.
```


### Phases


#### Draft Phase

A proposal enters the draft phase when staked `LQx` is first cosigned for it: this initiates the proposal. A governor parameter specifies the minimum required `LQx` that must be cosigned for the proposal to enter this phase.

Once the proposal is in the draft phase, stakers may *cosign* it with some of their staked `LQx`. For accounting purposes in this phase, any `LQx` that is cosigned *against* the proposal is ignored when determining if there is sufficient support for the proposal to move it to the voting phase: i.e., only `LQx` cosigned *in favour* of the proposal is tallied.


#### Review Phase

If a proposal in the draft phase gains sufficient cosigned `LQx` before it reaches the final `POSIXTime` for the draft phase, namely `T₀+D`, the proposal can transition to the review phase. If the proposal has not garnered a sufficient cosigning of `LQx` before that deadline, then it permanently transitions to the terminal "failed" state.

There is no on-chain activity for the proposal in the review phase. This phase simply provides a waiting period where potential voters can study the proposal. At the end of the review phase, the voting phase commences automatically.


#### Voting Phase

Locking `LQx` in the voting phase proceeds similarly to cosigning in the draft phase, but stakers *vote* either *in favour* or *against* the proposal. Furthermore, any `LQx` the previously was used to cosign the proposal is now automatically considered to be locked in favor of the proposal.

The net tally of `LQx` locked in favor minus `LQx` locked against is computed at the conclusion of the final `POSIXTime` of the voting period, and determines the success of the proposal. Sufficient net staked `LQx` results in the proposal transitioning to the locking phase, while insufficient net stake `LQx` results in it permanently transitioning to the terminal "failed" state.


#### Locking Phase

There is no on-chain activity for the proposal in the locking phase. This phase simply provides `LQUSD` and `LQx` holders an opportunity to prepare for the effects of the execution of the approved proposal.


#### Execution Phase

Once a proposal enters the execution phase, anyone can execute the effects (unless the proposal requires witnessing by particular signing credentials), as that can only be done in according with the proposal's validation logic. A governor parameter defines the deadline for ***initiating*** the proposal's execution. As the proposal is necessarily something that has been agreed by the voters, it will be incentivized naturally to be executed. Once the proposal has entered the execution phase, any staked `LQx` that was locked to it may be unlocked without effect. See [the specification on proposals](proposals.md) for a discussion of proposal types and execution.

```diff
- For LiqwidX, it seems likely that a proposal can be executed within a single
- slot. For Liqwid, however, it seems conceivable that execution might span
- several slots. If such is the case, does the deadline refer to initiating the
- execution or to completing it? If execution of a multi-slot proposal starts
- too late, is there a rollback of some sort? is proposal execution necessarily
- atomic?
```

```diff
- What about non-determinism in the execution phase where two or more
- conflicting or interacting proposals are approved and have overlapping time
- windows for execution? The final state might depend upon the sequence in
- which proposals happen to be executed within the interaction of their
- execution time windows. The draft of the proposal spec includes a provision
- that a proposal may clear the pipeline of proposals in previous phases,
- essentially forcing the proposers to re-draft those unexecuted proposals.
```


# Staking Pools

In order for an `LQx` holder to participate in the proposal processes, they must *stake* `LQx` in a *staking pool*. Conceptually, each stake holder has a *staking state* which consists of the following:

1.  The total amount of `LQx` that the staker has put in the staking pool.
2.  How much `LQx` they have cosigned to each draft proposal.
3.  How much `LQx` they have locked to each voting- or locking-phase proposal, and whether that vote is *in favour of* or *against* the proposal.
4.  Whether and to whom they have delegated control of their stake to ***another stake holder***.
5.  The pending, uncollected `ADA` rewards accumulated from staking, *or* the `LQUSD` wallet whose "free ADA" should be credited with staking rewards.

```diff
- Item 4 restricts delagation to be to other stake holders: this means that one
- cannot delegate to just anyone (e.g., someone who holds no `LQx` and is just
- a regular Cardano user.
```

A staker may add or remove `LQx` to the staking pool at any time, without any fees beyond the standard Cardano blockchain transaction fees. A staker may delegate, re-delegate, or un-delegate their stake at any time.

The following constraints apply to cosigned and locked `LQx`:

1.  `LQx` may freely be cosigned and un-consigned to a proposal during the draft phase.
2.  `LQx` that was cosigned to a proposal in its draft phase is considered to be locked in favour of the proposal for the review, voting, and locking phases.
3.  `LQx` may freely be locked and unlocked to a proposal during the review and voting phases.
4.  `LQx` that was locked to a proposal at the end of the voting phase may not be unlocked during the locking phase.
5.  Once a proposal enters the executation phase, all `LQx` locked to it is automatically unlocked.
6.  Only non-negative amounts of `LQx` may be cosigned or locked.
7.  The sum of all of a staker's cosigned and locked `LQx` may not exceed the amount they have staked.
8.  If the staker has delegated control of their stake to another stake holder, then the staker may not cosign, un-cosign, lock, or unlock `LQx`. Only the delegated stake holder may perform those actions.

Stakers receive [rewards](rewards.md) in proportion to their staked `LQx`. Delegation does not affect the rewards computations.
