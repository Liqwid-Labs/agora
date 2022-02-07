# Stakes and staking pool technical design

| Specification | Implementation | Last revision |
|:-----------:|:-----------:|:-------------:|
| WIP         |  WIP        | 2022-02-07    |

---

**Specification ownership:** [Emily Martins]

**Authors**:

-   [Emily Martins]
-   [Jack Hodgkinson]

**Implementation ownership:** [Emily Martins]

[Emily Martins]: https://github.com/emiflake

[Jack Hodgkinson]: https://github.com/jhodgdev

**Current status:** Originally written for Liqwid by [Emily Martins]. Rewritten by [Jack Hodgkinson]. Required review from [Emily Martins], especially the section on staking pools.

---

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

Altering the amount positioned in a stake is not possible, for as long as that stake is locked against any proposals. This is to prevent vote manipulation. Consider:

-   Ford stakes 100GT and votes in-favour of a proposal `p`. `p` now has +100GT in-favour.
-   Ford _increases_ his stake by 50GT to 150GT.
-   Ford retracts his vote from `p`. As his stake is 150GT, `p` deducts 150GT and `p` now has -50GT in-favour.

It should be clear how users could alter their stakes to reduce and manipulate the vote count. Preventing alteration of GT in stakes ensures that there is never a discrepancy between the amount of GT a user holds and the amount the system believes that they hold.

If a user wished to stake _more_ GT, they could always create a new stake that they would be free to lock on proposals. For this reason, it may be useful to include a method of 'merging' stakes, when they are not being locked against any proposals, to allow users to 'streamline' their GT portfolio.

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

## Staking pool

There are a number of reasons that a protocol would wish to track the global state of stakes in its system. For example, a protocol may wish to implement a proposal system where a certain proportion of the globally available GT must be staked in-favour of a proposal, in order to allow it to pass. This is the equivalent of mandating a certain voter turnout be met in a national referendum. The ability to do such a thing is conferred by the creation of a _staking pool_.

Whilst the ability to track stakes is a useful one, it is a complicated concept to implement on Cardano. A particular issue is _throughput and contention_: any purely on-chain solution is unlikely to be able to process data rapidly enough that the process of users creating stakes _and_ updating the staking pool is as seamless as desired.

One potential solution for this is an _escrow_ system, which implements a queueing solution by which users issue their stake and the system takes responsibility for updating the staking pool accordingly, with no further action required from the staker. This is a feature that is being considered for Agora v2.

Another implementation takes the burden of calculating the global state of user stakes _off-chain_. This allows the developers much more freedom in how they approach the solution, as they are no longer restricted by the complications of programming on a blockchain. What issue this solution _does_ have is one of **trust**. Any off-chain solution utilised by the protocol developers must demonstrate its fairness and accuracy to its users.
