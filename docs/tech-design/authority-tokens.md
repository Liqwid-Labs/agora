# Authority token technical design

| Specification | Implementation  | Last revision |
|:-----------:|:-----------:|:-------------:|
| WIP         |  WIP        | 2022-02-02    |

***

**Specification owner:** [Emily Martins]

**Authors**:

-   [Emily Martins]
-   [Jack Hodgkinson]

**Implementation owner:** [Emily Martins]

[Emily Martins]: https://github.com/emiflake

[Jack Hodgkinson]: https://github.com/jhodgdev

[Peter Dragos]: https://github.com/peter-mlabs

**Current status:**

Initially written up by [Emily Martins] for Liqwid. Transferred across to Agora. Rewritten by [Jack Hodgkinson]. Outstanding suggestions from [Peter Dragos] that require addressing.

***

## Authority tokens

In the spirit of extensibility, a governance system should be able to readily expand the effects proposals can have on their protocol. With Ethereum systems, this is often done with untyped data and references to specific contract hashes. [Compound](https://medium.com/compound-finance/compound-governance-5531f524cf68) encodes an array of the contracts' addresses (`address[] Proposal.targets`) and the data to be passed to them (`bytes[] Proposal.calldatas`). This does not translate to Cardano's EUTXO model, so we require an alternative approach.

Our approach relies on two core assumptions:

-   We trust the community to thoroughly assess the effects of a proposal. This ensures that the effects are safe and will work as intended.

-   The effects of a proposal may only alter the system, after their associated proposal has been passed by the community.

The first assumption is justified by the fact that any proposal stores the hash of its effects' codes, and these effects are available for any user to inspect. The community of GT-holders will be able to verify a proposal's purported benefits.

To ensure the latter assumption holds true, we introduce 'governance authority tokens' (GATs). Any effect that wishes to alter the system will be required to _burn_ a GAT, when it is enacted. These GATs will be issued to a proposal's effects via the governor component, which has the responsibility of ensuring that a proposal passed in a correct and valid manner. Conceptually, the ownership of a GAT by an effect is a way of demonstrating to a component: 'the DAO (decentralised autonomous organisation) has granted me permission to alter your parameters'.

> The components that need to be adjustable at a later point will need to allow this as means for proving authority and validation of a transaction. So, for example, a Liqwid market might need to have its parameters updated, the following diagram shows how this would happen after a proposal has successfully been voted on and passed:

This model naturally requires that any component of the system, desired to be adjustable via governance is aware of the existence and significance of GATs. For example, a [Liqwid](https://github.com/mlabs-haskell/liqwid-contracts/) market may be subject to the effects of a successful proposal to alter its parameters. The following diagram shows how such an update would occur:

![Governance Authority Token UTxO flow diagram](../diagrams/GovernanceAuthorityToken.svg)

> Peter, 2022-01-24: You should add a link to what conventions you're following for you UTXO flows to the README

> Peter 2022-01-24: I think I see what's going on here, but it could use some annotations/additional description.
> Tx1, a.) A proposal is closed via Tx1, and the transaction emits a continuing `Proposal` and `Governance` UtXO,
> along with an "Effect" UTxO (presumably identified in the content of the proposal). The Effect UTxO encodes effect `f` in it's validation logic.
> Question: where is this effect encoded?
> Tx1, b.) In the same transaction, a GAT is minted and paid to the UTxO.
> Tx1, c.) The `min utxo` ADA is paid to the Effect UTxO from "user inputs". I'm not certain where this is, but I'm assuming it's accumulated during voting or something? What is it used for?\
> Tx2, a.) The Effect UTxO and the component to be governed are consumed in transaction 2.\
> Tx2, b.) The Market's usual validation logic is short-circuited since the GAT was burned, and the continuing Market UTxO updates its datum.
> Tx2, c.) There is a continuing "Effect" UTxO; presumably, this serves as a proof that an effect has taken place? Meaning that a sequence of effects that require a particular order of execution can consume the output UTxO as a witness?
> Tx2, d.) The min ADA is paid to "user outputs"; is this just "collateral" to make sure that the effect is executed in a timely fashion?

This approach has a number of benefits but some important details must be kept in mind:

#### Handling multiple effects in a single proposal

Cardano script sizes are restricted to 16KB and previous proposal/effect models have had difficulties generating scripts which keep under this limit. By decoupling proposals and their effects, a proposal can have a far greater number of effects, as all it contains is a list of hashes for said effects. Effects themselves can make any number of changes to the system, as long as the resulting script is kept to an allowable size.

It is essentially impossible for the system to prevent conflicting effects taking place in short order and harming the system but the likelihood of this occurring is sufficiently reduced by all effects first having be passed by the DAO. There is scope for adding a notion of 'expiration', after which effects will no longer be able to enact their changes.

#### Writing effect code requires a _lot_ of care

The ability to alter large swathes of the system being conferred by the ownership of a single token poses the risk of (purposeful or inadvertent) harm. There are steps one can take to lessen this risk:

-   All validators for an effect transaction are specified **in full**. In comparison to the normal writing of validators, where one only specifies what they must, one must do all they can to avoid undesired behaviour being permitted by the validator.
-   All transactions are only executed by one of a few 'trusted' DAO members. This would necessarily have to be encoded in the effect.

This should be sufficient to prevent loopholes. Delegating authority through validation or movement of tokens is often a necessary risk in blockchain systems and this problem is no more complex than others of its kind.

There is scope for expanding GATs to make them more restrictive and therefore less dangerous. One proposal is to store the hash of the effect script within the GAT, thus allowing for the minting of 'bespoke GATs', which are only authorised to make changes allowed for in the effect hash.
