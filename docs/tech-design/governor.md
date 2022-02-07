# Governor technical design

| Specification | Implementation    | Last revision |
|:-----------:|:--------------:|:-------------:|
| WIP         |  WIP           | v0.1 2022-02-03 |

---

**Specification ownership:** [Jack Hodgkinson]

**Authors**:

-   [Jack Hodgkinson]

**Implementation ownership:** _unassigned_

[Jack Hodgkinson]: https://github.com/jhodgdev

**Current Status**:

First draft. Subject to review by @emiflake.

---

The governor is a simpler component than others in the system but still provides an invaluable role. Firstly, it acts as a repository for all the governance parameters e.g. the duration of proposal phases or GT thresholds. Secondly, and perhaps most importantly, it verifies whether proposals have passed legally and, if they have, grants their [effects](effects.md) powerful [governance authority tokens](authority-tokens.md).
