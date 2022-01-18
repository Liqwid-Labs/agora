# StakingPool technical design

| Spec Status | Impl Status | Last Revision |
|-------------|-------------|---------------|
| WIP         |  WIP        | 2022-01-18    |

--------------------

**Spec Ownership:** [@Emily Martins]

**Authors**: [@Emily Martins]

**Impl Owner:** [@Emily Martins]

[@Emily Martins]: https://github.com/emiflake

--------------------

## The StakingPool

In order to be able to count votes at all, some means of proving a user's skin in the game on-chain must exist. We propose having a central StakingPool contract which mints separate per-user UTXOs in which the governance token can be deposited. The MintingPolicy of the state threads ensures that it is paid to the script and with valid initial state. This circumvents the need for a central token, and makes the minting of such tokens concurrently possible.

### Stake UTXOs

A stake UTXO stores the information to allow accessing your staked GT as if it was a safe.

```haskell
data Stake = Stake
  { -- | Which proposals has this Stake been used to vote on?
    lockedByProposals :: [(DatumHash, ProposalVote)]
  , -- | The amount staked by this utxo
    stakedAmount :: GT
  , -- | Who owns this Stake
    owner :: PubKeyHash
  }
```

When voting for a proposal, the Stake UTXO is used to witness the user's staked amount. As a result, the two following state transitions take place (pseudocode):

```haskell
proposal.votes += (stake.stakedAmount, vote)
stake.lockedByProposals += (hash proposal.settings, vote)
```

A sort of mutual binding between the proposal and the stake is created and undoing one undoes the other, which is exactly what we want!

Depositing and withdrawing is made illegal when `stake.lockedByProposals` isn't empty. Withdrawing is illegal so that you can't have GT in a vote, without having it anymore, whereas Depositing is illegal so that you can't deposit after a vote and unvote it again in order to retract more than you originally voted. Thus preserving that

#### Delegating stake

Most things like Cosigning sort of work trivially by just witnessing Stake, but delegation requires an extra step. We add a field to what `Stake` stores.

```haskell
data Stake = Stake
  { -- | Which proposals has this Stake been used to vote on?
    lockedByProposals :: [(DatumHash, ProposalVote)]
  , -- | The amount staked by this utxo
    stakedAmount :: GT
  , -- | Who can spend our utxo for us when voting
    delegatedTo :: Maybe ValidatorHash
  , -- | Who owns this Stake
    owner :: PubKeyHash
  }
```

We simply link one stake to another. When voting now the voter can check which Stake utxos are delegated to them off-chain, and include them in the transaction for voting. **This will lock the delegators' utxos**, but that's no big deal because they can themselves unlock it just like usual. The validity of the hash provided to the Stake is irrelevant. It simply delegates its _authentication_ to the particular hash. Note, it only delegates authentication for _voting_ but not for example for withdrawing.
