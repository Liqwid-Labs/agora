# Governance concepts

This document seeks to introduce to the reader the concept of governance systems on Cardano and acquaint them with the core components of a generic governance system.

## Simple questions, simple answers

### Q: What's a 'governance'?

A: A _governance system_ is a component of a Cardano system, that allows for its community to issue and vote on proposals.

### Q: What can be proposed?

A: That depends on the Cardano protocol in-question. An example for a proposal could suggest that funds be released from a community treasury. Another might propose an alteration to some parameter in the wider-system.

### Q: Who can vote?

A: The right to vote is conferred by the _staking_ of some designated _governance token_. If one owns the relevant token, they may 'stake' some of it to vote in favour of, or opposition to, some proposal. The 'weight' of their vote will be directly proportional to the amount they stake on it.

## Overview of components

More-detailed information on individual components will be included in their own, specific documentation. This section provides brief descriptions on the purpose of each component.

### Proposal

A proposal suggests for some specified changes to be made to a Cardano system. It is voted upon by the community and, if passed, its effects are applied to the system.

### Governor

The governor may be conceived of as the 'centre' of a given 'governance system'. Users submit proposals to the governor, which creates them on-chain and keeps track of them. Furthermore, it holds parameters which affect the behaviour of the governance system e.g. the duration of a proposal's voting phase. Finally, it issues 'governance authority tokens', which are used to permit changes to the system.

### Governance tokens

Governance tokens (GTs) are a currency, which confer the right to vote on proposals. The more GTs one stakes, the more their vote counts on proposals. 

### Staking pool

A staking pool is required, so that the system knows how much 
