# Safety Pool functionality for the Staking Pool

| Specification | Implementation | Last revision |
|:-----------:|:-----------:|:-------------:|
| WIP         |  WIP        | 2022-05-13    |

---

**Specification ownership:** [Emily Martins]

**Authors**:

-   [Emily Martins]

**Implementation ownership:** [Emily Martins]

[Emily Martins]: https://github.com/emiflake

[Jack Hodgkinson]: https://github.com/jhodgdev

**Current status:** Early revision of the document with low technical specification due to feature being further in the timeline. Has been reviewed by [Jack Hodgkinson].

---

In order for Agora’s staking pool to act as a *safety pool*, it needs to be able to support a workflow for slashing staked governance tokens (GT) to act as a safety mechanism. (Note: we use "slashing" to mean taking away some token from a particular user.) This document outlines the changes that need to be made to Agora in order to support this.

### Motivation

In the event of a protocol suffering loss of funds through a [shortfall event](https://docs.aave.com/aavenomics/safety-module#shortfall-events), slashing a percentage of locked GT can be used to attempt a recovery. Ultimately, doing this is beneficial for the stakeholders because it allows the protocol to recover and eventually benefits them as well (even though they bear the initial cost). Striking a balance (in the form of the right percentage slashed) is important in order for stakeholders to want to vote in favour of a proposal that results in such a slashing.

## Slashing functionality

In order to allow an admin to withdraw a set percentage of the amount staked, we create a new effect.

### The `SlashEffect` validator:

- Mint a `SlashToken` and send it to a validator ("the `Slash` validator") with a datum encoding the details of the slashing.

  The `SlashDatum` could look like this:

  ```haskell
  data SlashDatum = SlashDatum
    { -- | Identify which slash event this datum belongs to.
      slashId :: Integer
    , -- | Represents how much is to be slashed (as a ratio of the full staked amount).
      slashPercentage :: Rational
    , -- | The time range that must contain `txInfoValidRange` in order to slash.
      slashTimeRange :: POSIXTimeRange
    }
  ```

- `SlashDatum` must, in some way, be present in the datum that is passed to the `SlashEffect` validator. This means that the `ProposalDatum` also indirectly contains `SlashDatum`.

### The `SlashToken` policy:

- Exclusively check for GAT burn. Delegated checking goes to the `SlashEffect` validator.
- This `SlashToken` policy needs to be "known" by the Stake validator, in order to allow transactions to take place.

### The `Slash` validator:

- This validator allows spending of a percentage of a `Stake`s GT, provided a few conditions are met:
    - The `SlashToken` is present
    - The slash ID is tagged onto the new stake datum
    - The time range encoded in the `SlashDatum` includes the `txInfoValidRange`.
- What is done with the recovered GT is up to the admin to determine. Q: Is this what we want?

Finally, we need to change `StakeDatum` to encode a list of slash IDs in order to prevent slashes happening twice.

---

## Preventing opting out of slashing

If this is where we call it quits, then users will each be able to just opt-out of this slashing event. GT holders are individually incentivized to do so, because it means they don’t forfeit their own assets. Obviously, then, in order to make the safety pool work at all, we need to prevent this.

### Time-locking stakes

A simple solution is time-locking stake withdrawal upon any interaction with it for a set amount of days. This ought to be long enough for a full proposal to go through, but not too long for it to become annoying for users of the staking pool. This presents a big drawback in general for all stakeholders as their assets are actually locked even though no slashing necessarily will ever happen. However, this is also a very simple solution for solving the opt-out problem. It should be something we can enable or disable after the fact, as well as in initial configuration.

### CIP-31 dependent central lock

Provided we have reference-inputs ([CIP-31](https://cips.cardano.org/cips/cip31/)) by the time we implement this, an alternative approach is viable:

- We create a script that manages a `StakeLockDatum`. The script (”`StakeLock` validator”) encodes whether or not `Stake`s are allowed to withdraw. Using reference-inputs, we are able to witness this datum without consuming it, allowing us to lose no throughput on withdrawals, while maintaining a centralized lock.
- The `StakeLock` validator can only set to lock through an admin-controlled multisig. The admin multisig should do this in the event that a proposal has been created for the shortfall event.
- The `StakeLock` utxo can be consumed by anyone after a set period of time, unlocking it again. This prevents admins from abusing the locking for whatever reason.
