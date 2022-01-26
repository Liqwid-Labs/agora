# Authority token technical design

| Spec status | Impl status | Last revision |
|-------------|-------------|---------------|
| WIP         |  WIP        | 2022-01-18    |

***

**Specification owner:** [Emily Martins]

**Authors**: [Emily Martins]

**Implementation owner:** [Emily Martins]

[Emily Martins]: https://github.com/emiflake

***

## Authority Tokens

In the spirit of extensibility, a governance system should be able to readily expand the effects proposals can have on their protocol. With Ethereum systems, this is often done with untyped data and references to specific contract hashes. [Compound](https://medium.com/compound-finance/compound-governance-5531f524cf68) encodes an array of the contracts' addresses (`address[] Proposal.targets`) and the data to be passed to them (`bytes[] Proposal.calldatas`). This does not translate to Cardano's EUTXO model, so we require an alternative approach.


> In order to allow this flexibility, there are two facts that we rely on:

-   We trust the community to validate the proposal entirely, including whether or not the effects encoded in it are written correctly. (This may mean we have a set of known and trusted effects we agree are correct and safe, collectively)
-   The effects are given authority only after the proposal that promises them succeeds.

Our approach relies on two core assumptions: 

- We trust the community to thoroughly assess the effects of a proposal. This ensures that the effects are safe and will work as intended.

- The effects of a proposal may only alter the system, after their associated proposal has been passed by the community.

To achieve the former is rather simple, the effect validator's source code is available for anyone to look at, and it hashes correctly to the hash stored in the proposal itself. So, the LQ holders can decide on whether it is a positive for the system.

> Peter, 2022-01-24: At this point in reading, its not obvious that "effect"s have validators. This is likely explained elsewhere, but this should be kept in mind when considering the order in which these specs should be read.

> Peter, 2022-01-24: Update: after some more reading, it sounds like the Effect/GAT pair is basically a way to short-circuit the "normal" validation of a UTxO at one of the component addresses; it basically says "ignore your usual validation logic; I have a GAT, so this transaction must be approved".

In order to achieve the latter, we must introduce some way to give effects authority to perform their actions.

We do this by handing out "Governance Authority Tokens" (GATs) to each of the the effects belonging to a proposal after the proposal passes. When these authority tokens are _burned_, they act as a way of saying "the DAO validated this, so trust that I will ensure this transaction is correct".

The components that need to be adjustable at a later point will need to allow this as means for proving authority and validation of a transaction. So, for example, a Liqwid Market might need to have its parameters updated, the following diagram shows how this would happen after a proposal has successfully been voted on and passed:

> Peter, 2022-01-24: RE: "(...) need to be adjustable at a (...)": could it make more sense to say "adjusted via governance"?

![Governance Authority Token UTxO flow diagram](../diagrams/GovernanceAuthorityToken.svg)

> Peter, 2022-01-24: You should add a link to what conventions you're following for you UTxO flows to the README

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

As a result of this approach, there's a number of benefits, but also details we need to watch out for:

#### Handling multiple effects in a single Proposal

Handling multile effects this way is very doable. For one, a single effect can do multiple things at once, if the script sizes allow it. But another point is that a proposal can have a _list_ of hashes that it distributes GATs to. And this is all without growing size almost at all, because the actual effects are encoded in their scripts.

There is the concern of expiration date of effects and incomplete execution now, however. But the customizability allows for failsafes (like retrying) and encoding expiration correctly. Due to the nature of these effects being handled by the DAO, it's assumed no conflicting effects will be executed in short succession. This is essentially impossible to encode in the system itself, so this tradeoff must be taken.

#### Writing effect code needs a _lot_ of care

Having delegated the authority of _the entire system_ in a single token is a lot of power for one tiny script. And with great power comes great responsibility. It is important that this token doesn't fall into the wrong hands or is executed in a way that was unexpected (by the community). There's a few things we can do to help mitigate this:

-   We write the validator with the transaction in mind specified out in fullest. No extra inputs, no extra outputs, no extra mints, etc. Essentially this is the opposite of what we do in other places, where we try to be only as specific as we need.
-   We have the transaction be executed by one of a number of community trusted members. This of course is something that is encoded in the effect, rather than anywhere else.

Hopefully, this is sufficient to ensure the transactions are created correctly, and nothing unexpected slips in. This problem is no more complicated or dangerous than elsewhere where we delegate certain trust or authority to just the validation or movement of a token.

> 2022-01-24, Peter: Have you considered placing the validator hash of the effect script in the token name? This could tie a proposal to a GAT to a validator, instead of having any GAT work with any effect script.

> 2022-01-24, Peter: This section could use some additional explaination/pseudo-code of the what the GAT minting policy would look like and what additional validation logic would go into a govern-able component
