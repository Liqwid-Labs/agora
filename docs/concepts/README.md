| Specification | Last Revision |
|:-------------:|:-------------:|
| Draft      | v0.1, 2022-02-01 |

***

**Specification ownership:** [Jack Hodgkinson]

**Authors**:

-   [Jack Hodgkinson]
-   [Emily Martins]

**Current status**:

Considering potential expansion of 'Simple questions, simple answers' section. Awaiting review from @emiflake.

[Jack Hodgkinson]: https://github.com/jhodgdev

[Emily Martins]: https://github.com/emiflake

***

# Governance concepts

This document seeks to introduce to the reader the concept of governance systems on Cardano and acquaint them with the core components of a generic governance system.

## Simple questions, simple answers

### Q: What's a 'governance'?

A: A _governance system_ is a component of a Cardano system, that allows for its community to issue and vote on proposals.

### Q: What can be proposed?

A: That depends on the Cardano protocol in-question. An example for a proposal could suggest that funds be released from a community treasury. Another might propose an alteration to some parameter in the wider-system.

### Q: Who can vote?

A: The right to vote is conferred by the _staking_ of some designated _governance token_ (GT). If one owns the relevant token, they may 'stake' some of it to vote in favour of, or opposition to, some proposal.

## Overview of components

![Governance overview diagram](/docs/diagrams/gov-overview.svg "Governance overview diagram")

More-detailed information on individual components will be included in their own, specific documentation. This section provides brief descriptions on the purpose of each component.

### Users

A user is a member of the DAO (decentralised autonomous organisation), who
may choose to redeem governance tokens (GT) from the treasury and stake those GT
to vote in favour of, or opposition to, proposals.

### Governance tokens

Governance tokens (GTs) are a currency, which confer the right to vote on proposals.

### Treasury

The treasury of a governance system is responsible for determining which users are entitled to what GT rewards and when. If a user is eligible for a reward, it they must claim it from the treasury. A treasury can also serve as a form of 'DAO wallet', storing and saving funds that can be later spent by the community.

### Stakes

Users are required to 'lock' their GT in stakes, so that the system has some idea of their eligibility to vote on proposal.

### Staking pool

A staking pool can be introduced, for the purpose of tracking the aggregate status of all stakes in the system. Some systems may not require a staking pool.

### Proposal

A proposal suggests for some specified changes to be made to a Cardano system. It is voted upon by the community and, if passed, its effects are applied to the system.

### Effects

Proposals may have one or many 'effects', which represent the concrete, individual changes the proposal would make to the system. If their corresponding proposals have been passed, each effect is granted a _governance authority token_ (GAT) by the governor. This token permits the effect to alter the system.

### Governor

The governor is responsible for validating whether proposals have passed. If they have, it issues GATs to the proposals' effects.
