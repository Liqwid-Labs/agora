# Governor technical design

| Specification | Implementation    | Last revision |
|:-----------:|:--------------:|:-------------:|
| WIP         |  WIP           | v0.1 2022-02-03 |

---

**Specification ownership:** [Jack Hodgkinson]

**Authors**:

-   [Jack Hodgkinson]
-   [Emily Martins]

**Implementation ownership:** _unassigned_

[Jack Hodgkinson]: https://github.com/jhodgdev

[Emily Martins]: https://github.com/emiflake

**Current Status**:

First draft. Subject to review by @emiflake.

---

The governor is a simpler component than others in the system but still provides an invaluable role.

### Governor responsibilities

The governor acts as the central authority through which all governance proceeds indirectly. You can think of it as the single point for issuing rights to actors.

#### Minting of authority tokens

The main responsibility for the governor is allowing the minting of [authority tokens](authority-tokens.md), and ensuring they are sent to [effects](effects.md). The governor needs to check that this happens as a result of a proposal finishing its voting phase.

#### Guarding creation of proposals

The governor is also responsible for allowing the _creation_ of proposals. There are a few rules for the creation of a proposal, and the governor ensures these rules are followed.

In order for proposals to have sequential IDs, the governor keeps track of a counter which it uses to ensure the ID has not been taken yet. The governor also needs to keep global settings that influence how proposals behave: thresholds for state transitions, how long each phase lasts, etc.

The governor itself is sensitive to authority token burning in order to allow all of this to be able to mutate.
