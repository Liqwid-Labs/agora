# Agora

Agora is a set of Plutus scripts that compose together to form a governance system.

### What is Agora

Goals:

-   Agora aims to reduce duplication in Liqwid and LiqwidX and to serve as a one-size-fits-all governance library for projects on the Cardano blockchain.
-   Agora aims to be modular and flexible for specific needs but presents an opinionated architecture.

Non-goals:

-   Agora is not a DAO. It doesn't have tokenomics or even a token. It is simply a library for governance.
-   Agora doesn't aim to provide any primitive tools for Plutus that are not governance-specific. For this, see [plutus-extra](https://github.com/Liqwid-Labs/plutus-extra/).

## Project setup

An up to date version of the [`nix` package manager](nixos.org) (>=2.3) is required to build this project. For information on how to install, see the [nixos website](https://nixos.org/download.html). Important: See also [this section](https://github.com/input-output-hk/plutus#nix-advice) on binary caches.

Open a dev-shell with `nix develop` and build with `cabal build`.

## Documentation

Documentation for Agora may be found in [docs](./docs).

## Road-map

### v1

-   [ ] Governor
-   [ ] Treasury
-   [ ] Staking pool
-   [ ] Proposals
-   [ ] Effects

### v2

-   [ ] Rewards distribution

### Beyond

-   [ ] ...


# Governance concepts

This section seeks to introduce to the reader the concept of governance systems on Cardano and acquaint them with the core components of a generic governance system.

## Simple questions, simple answers

### Q: What's a 'governance'?

A: A _governance system_ is a component of a Cardano system, that allows for its community to issue and vote on proposals.

### Q: What can be proposed?

A: That depends on the Cardano protocol in-question. An example for a proposal could suggest that funds be released from a community treasury. Another might propose an alteration to some parameter in the wider-system.

### Q: Who can vote?

A: The right to vote is conferred by the _staking_ of some designated _governance token_ (GT). If one owns the relevant token, they may 'stake' some of it to vote in favour of, or opposition to, some proposal.

## Overview of components

<p align="center">
  <img src="/docs/diagrams/gov-overview.svg"/>
</p>

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
