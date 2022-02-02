# Staking pool technical design

| Specification Status | Implementation status | Last revision |
|:-----------:|:-----------:|:-------------:|
| WIP         |  WIP        | 2022-02-02    |

***

**Specification ownership:** [Emily Martins]

**Authors**:

-   [Emily Martins]
-   [Jack Hodgkinson]

**Implementation ownership:** [Emily Martins]

[Emily Martins]: https://github.com/emiflake

[Jack Hodgkinson]: https://github.com/jhodgdev

**Current status:** Originally written for Liqwid by [Emily Martins]. Rewritten by [Jack Hodgkinson]. Will be subject to changes, when a final implementation for the staking pool has been decided.

***

## Stake UTXOs

A 'stake' records how much GT has been staked by a user, and on which proposals it has been locked against.

```haskell
data Stake = Stake
  { -- | Which proposals has this stake been used to vote on?
    lockedByProposals :: [(DatumHash, ProposalVote)]
  , -- | The amount of GT in the stake.
    stakedAmount :: GT
  , -- | The owner of the stake.
    owner :: PubKeyHash
  }
```

When voting for a proposal, the stake UTXO is used to witness the user's staked amount. As a result, the two following state transitions take place:

```haskell
proposal.votes += (stake.stakedAmount, vote)
stake.lockedByProposals += (hash proposal.settings, vote)
```

This forms a mutual binding between the proposal and the stake.

A stake may be used to vote on an unlimited number of proposals. Consider a user staking 50GT. They may pledge that 50GT against a proposal `p` _and_ another proposal `p'`.

Altering the amount positioned in a stake is not possible, for as long as that stake is locked against any proposals. This is to prevent two potential malpractices:

1.  A user stakes `n` GT and votes on a proposal. They then withdraw their stake and sell it on a DEX. They no own zero GT but have `n` GT staked on a proposal.
2.  A user stakes `n` GT and votes on a proposal. They further deposit `k` into their stake. They revoke their vote and redeem `n + k` GT, leaving them with a `k` GT profit.

Preventing alteration of GT in stakes ensures that there is never a discrepancy between the amount of GT a user holds and the amount the system believes that they hold.

## Delegating stake

Aspects of delegation, such as co-signing work through the trivial witnessing of a stake UTXO, however allowing a user to delegate their stakes requires an extra field:

```haskell
data Stake = Stake
  { -- | Which proposals has this stake been used to vote on?
    lockedByProposals :: [(DatumHash, ProposalVote)]
  , -- | The amount of GT in the stake.
    stakedAmount :: GT
  , -- | The owner of the stake.
    owner :: PubKeyHash
  , -- | To whom this stake has been delegated.
    delegatedTo :: Maybe ValidatorHash
  }
```

When voting on a proposal, a user can check which stakes have delegated to them off-chain and include them in the voting transaction. _This will lock the delegator's stake_, however they will be themselves be able to unlock it as usual. It should be noted that delegation of stakes only extends to voting on proposals and not, for example, withdrawing GT from a stake.
