# Using Agora

## Motivation

If you are building a project on Cardano that involves decentralized interaction between users you may want to create a DAO ([Decentralized Autonomous Organization](https://www.wikiwand.com/en/Decentralized_autonomous_organization)).

A DAO will allow users to come to a consensus on a variety of matters relevant to your project. These could include: managing of treasury assets, changing of protocol parameters, replacing of scripts, deprecation of the protocol in favour of a new version, emergency actions to protect users, and so forth. In order to do this on-chain, users will have to be able to express their opinion contractually, and only those with a vested interest ought to be able to interact with relevant proposals. This should ensure that voters have the best interests of the protocol at-heart. Governance systems can take varied forms, and not all of them will be suitable for your project.

Building such a system is a complex process and requires a lot of care; ensuring fairness (with regard to user interactions), efficiency (with regard to contention and throughput) and simplicity (with regard to script size and transaction costs). Agora is a curated set of scripts, types and design patterns that are designed from the ground-up to solve this problem in a way that is flexible enough to suit essentially any protocol.

### A quick note on terms

This article will include common English words that have specific meanings in Agora. To help you disambiguate, here are some definitions:

- proposal: A collection of changes to the protocol, which are voted on as a block.
- effect: An on-chain representation of a proposed change to the protocol. A 'proposal' will hold references to one or many 'effects'. If an effect's proposal is passed by the community, effects are granted special 'authority tokens' which permit them to enact their encoded changes to the relevant protocol components.

![Proposals have effects, which alter components.](/docs/diagrams/UsingAgora.svg)

## Agora and your protocol

Agora’s staking model relies on the existence of a governance token. In a sense, this governance token _parameterizes_ the entire system. Agora staking pools will lock users' governance tokens in order to permit them to vote. Agora's components are free-standing and don't _require_ a protocol acting in a particular way in order to function. One could for instance technically create a DAO that works with ADA as its governance token. The tokenomics of your governance token will of course influence the way voting power is distributed, due to the nature of token-based voting.

In order to set-up your protocol’s DAO actions, all affected components of your protocol will need to interpret the burning of an _authority token_ as a licence to alter _any_ aspect of that component.

To put this in slightly more concrete terms: for any datum which holds a subset of your protocol's parameters, there _should_ exist a redeemer for that datum and validation for this datum/redeemer pair should do _no more_ than verify that one of these authority tokens has been burned. Without this flexibility, the effects of Agora's proposals would be markedly less powerful.

### Writing effects

One writes a proposal effect, as one would write any Plutus script with the caveat that the effect script will only be permitted to run _once_.

Consider an example NFT project, wherein the minting of each NFT is a community action. For this scenario, one would require a template `MintNFT` effect, which mints its corresponding NFT upon the passing of the relevant proposal. The proposal being passed will issue an authority token to the effect. Each NFT's policy will verify that such an authority token was burned upon minting, which demonstrates that the minting of the NFT was indeed authorized by the DAO.

Making your protocol's components aware of authority tokens, and implementing relevant effects are the only two chores of using Agora in practice. The former is the only one that involves adapting your own scripts. Effect scripts can be written after your protocol and its governance have deployed, provided that the authority tokens are respected by the components.

## What Agora leaves up to you

Agora’s concern is the on-chain components and scripts. Any front-ends are the concern of the protocol's developers. In the best case, our documentation and program design will inspire you in developing a front-end solution. There is scope for Agora containing some off-chain functionality in-future. This would allow the user to create and experiment with transactions.

It’s worth noting that, while the actual functionality of the _front-ends_ is not a concern, creating standards for off-chain metadata _is_. For example, metadata tagging proposal descriptions, tags, dates. These are all important features that Agora aims to standardize, in hopes of facilitating interoperability between various instances of Agora. This effort is similar to [CIP-25](https://cips.cardano.org/cips/cip25/), which aims to standardize metadata for NFTs.

You're welcome to write any new effects you require for your protocol. If you believe any effects you write are sufficiently general and could serve as a benefit to our community, we would encourage you to up-stream them. Guidelines for doing so may be found in our [contribution guide](/CONTRIBUTING.md). Agora provides a number of effects out-of-the-box and intends to add more with time.

## What to do if something is missing

In the event Agora does not provide for one of your use cases, feel free to raise an issue and we can begin a discussion on implementing the desired functionality. Our [contribution guide](/CONTRIBUTING.md) has guidelines for registering issues.
