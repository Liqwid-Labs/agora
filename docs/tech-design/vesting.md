# Vesting contract technical design

| Specification | Implementation    | Last revision |
|:-----------:|:--------------:|:-------------:|
| WIP         |  WIP           | v0.1 2022-01-26 |

***

**Specification ownership:** [Emily Martins]

**Authors**:

-   [Emily Martins]
-   [Jack Hodgkinson]

**Implementation ownership:** [Emily Martins]

[Jack Hodgkinson]: https://github.com/jhodgdev

[Emily Martins]: https://github.com/emiflake

**Current Status**:

***

## Vesting contract

In order to distribute governance tokens, one or more vesting contracts may be deployed.

First and foremost, there is a _schedule_ for the distribution of said tokens. Alongside this schedule, we keep track of how much we've distributed so far. This allows us to calculate how much is "due" at any particular time.

To gain intuition, you can think of it being implemented this way:

```haskell
distributed :: Schedule -> Time -> Value
distributed schedule time = scanl (+) mempty schedule !! time

due :: Schedule -> Value -> Time -> Value
due schedule alreadyDistributed time = distributed schedule time - alreadyDistributed
```

The vesting contract may require that only an address in a particular set can withdraw from it.

The vesting contract may have an optional escape hatch for trusted authority to recover from a potential DAO operation.
