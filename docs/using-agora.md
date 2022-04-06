# Using Agora

### Motivation

If you are building a project on Cardano that involves decentralized interaction between users you may want to create a DAO (Decentralized Autonomous Organization). A DAO will allow users to come to consensus on various matters relevant to your project. For instance managing of treasury assets, changing of protocol parameters, replacing of scripts, deprecation of the protocol in favour of a new version, emergency actions protecting the users, etc. In order to do this on-chain, users will have to be able to voice their opinion contractually, and only those with skin in the game ought to be able to interact, so they interact in favour of the protocol at large. Furthermore, a balance must be struck when it comes to the various features of governance and your particular protocol. Various flavours of governance exist, each with their own trade-offs.

Building such a system is quite a complex process and requires a lot of care; ensuring fairness (with regard to user interactions), efficiency (with regard to contention and throughput) and simplicity (with regard to script size, tx costs). Agora is a curated set of scripts, types and design patterns that are designed from the ground up to solve this problem in a way that is flexible enough to suit essentially any protocol while also ensuring a balance is struck in trade-offs by default. Hopefully this will save time.

### Agora and your protocol

Agora’s staking model relies on the existence of a governance token. The entire system essentially is “parameterized” by your governance token. The Agora staking pools will lock user’s governance tokens in order to allow them to vote. However, the majority of Agora components can live on their own after that fact. One could for instance technically create a DAO that works with ADA as its governance token.

In order to wire up your protocol’s DAO actions:

- All relevant and affected parts of your protocol will need to enable authority token burns to act as full authority:

In the case of, say, a datum that stores parameters of your protocol, there ought to be a redeemer that delegates the validation of the entire transaction to a single check for the burning of the authority token. This allows flexibility of proposal outcomes, and is the core building block for Agora’s effects.

- Write effects that perform desired actions within your protocol.

Writing an effect is as simple as writing any other script, but this script has only a single transaction in mind. Let’s say you’re building an NFT project, and you want to leave all mints up to the community to decide on. In this scenario you could make a `MintNFTs` effect which will mint the NFTs after a proposal passes. This is where the authority token burn “master key” comes into play: The policy of the NFTs can check for an authority token burn, proving it was “authorized” by the DAO.


These two are the only required chores of using Agora in practice. The former is the only one that involves adapting your own scripts. Effect scripts can be written after your protocol and its governance has deployed, provided the authority tokens are respected by the components.


### What Agora leaves up to you

Agora’s bread and butter is the on-chain components and scripts. So, the frontend is a concern of the consumer of the library. Hopefully the documentation and design will be enough for you to figure out the best way to design your frontends. In the future, an off-chain library may exist for Agora, which would contain various functions for creating transactions.

It’s worth noting that, while the actual functionality of the *frontends* isn’t a concern, documentation on standardization of off-chain metadata *is*. For example, off-chain metadata tagging of proposal descriptions, tags, dates, etc. These are all important features that Agora aims to standardize, in hopes of helping the interoperability between various protocols, and DAOs for DAOs.

Writing new effects that are protocol specific is up to you too. Although, if your protocol is general in nature, and you believe that an effect can be beneficial to other users of Agora, then you can contribute it. Agora ships with a number of meta-effects out of the box, and intends to add more as time goes on.

### What to do if something is missing

If you find that one of your use cases isn’t covered by Agora, or there is a hole in the spec with regards to potentially critical importance to your project. Then feel free to create an issue for it and we will open discussion on that matter. See [CONTRIBUTING.md](../CONTRIBUTING.md) before doing so.
