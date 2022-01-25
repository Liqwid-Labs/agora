# Proposals Technical Design


| Spec Status | Impl Status | Last Revision |
|-------------|-------------|---------------|
| Superseded by Agora    |  Superseded by Agora        | 2022-01-25    |

--------------------

**Spec Ownership:** N/A

**Authors**: [@Tilde Rose] [@B W Bush]

**Impl Owner:** N/A

**Current Status**:

This spec was written by Tilde/Brian Bush for the LiqwidX project in 2021, but was left unfinished. 
The current efforts in this direction have moved into Agora. 
Nothing in this document should be taken as authoritative or current; this document is being temporarily
preserved in case some of the material within is useful in Agora.


[@B W Bush]: https://github.com/bwbush

--------------------


```diff
- The bold+italic text below indicates content that might be controversial or
- that attempts to unilaterally resolve or reconcile ambiguities and conflicts
- in the source documents.
```


## Summary

LiqwidX proposals have the capability to alter governance and market parameters, to disperse funds from the LiqwidX treasury, or upgrade the LiqwidX system. They are initiated, drafted, voted on, and approved within [LiqwidX's governance](governance.md) process.


# Proposal Capabilities

Proposals may do the following:

1.  *Parametric proposals:* Make parametric alterations that do not change validators.
    a.  Alter the governance parameters that define the voting and proposal-approve processes.
    b.  Alter the parameters that specify the `LQUSD` market or the `LQx` stakepools.
    c.  Alter public key hashes for trusted parties such as multisig holders and oracles.
2.  *Distribution proposals:* Distribute funds from the LiqwidX treasury.
3.  *Upgrade proposals:* Upgrade the LiqwidX system by replacing validators.
4.  *Custodian proposals:* Direct action on the part of LiqwidX multisig custodians.

Proposals are considered "monolithic" in that they may perform any combination of the above in a single proposal. Additionally, a proposal's execution may optionally clear the pipeline of all other proposals currently under consideration (i.e., proposals in any phase, including those approved for execution but not yet executed). Furthermore, a proposal may optionally freeze the state of the system while it is being executed. Finally, proposals will contain metadata that provides a non-normative textual description of the proposal's intent and effects.

```diff
- It is important to let a proposal's execution remove all pending proposals,
- so in order to avoid unforeseen and path-dependent (non-deterministic)
- interactions between proposals. A simple parameter update wouldn't need to
- exercise this option, but proposals
- Are their other alterations that LiqwidX proposals can make? Are there stake
- pool parameters that could potentially be altered by a proposal?
```


## Parametric Proposals

A proposal may alter governance, market, stakepool, and any other parameters in the LiqwidX state. Those parameters may be numeric values, public key hashes, etc. These *parametric proposals* are specified by the validators for the transactions that will execute the proposal's effects. Once approved, anyone can execute these validators.


## Distribution Proposals

A *distribution proposal* may dispense `LQx`, `ADA`, and other assets from the treasury to one or more script or public-key hash addresses, without limitation aside from the availability of funds. Once approved, anyone can execute these validators.


## Upgrade Proposals

A system-upgrade proposal replaces the governor state-machine with a new one, and orchestrates an orderly migration from obsolete validators (and their script addresses) to new ones. An upgrade may be as simple as replacing a single validator for a single aspect of LiqwidX, or it may completely redefine the protocol. The simpler upgrade proposals might be executed by anyone, but more complex ones would likely involve signatures by the multisig custodians.

```diff
- We could split this category into simple upgrades (e.g., replacinng a few
- validators) versus complex upgrades (substantial protoocol changes that
- reinvent the system). The former can be executed quite simply and quickly,
- but the later might involve extensive migration. These could be called
- "upgrade" versus "protocol" proposals.
```

Upgrade proposals must properly migrate governance and other tokens to the new protocol and they must preserve the value in the system. Presumably, such proposals would be thoroughly audited before they enter the draft phase.

```diff
- This isn't the section to specify implementation mechanisms, but a single
- level of indirection and the use of script proxies could be adequate for
- upgrade proposals. Some of these proposals may require large numbers of
- transactions to migrate everything, but perhaps lazy migration might be
- possible.
```


## Custodian Proposals

Especially in the early stages of the LiqwidX DAO, it is likely that some operations might be handled by trusted custodians operating in concert within a multsig context. *Custodian proposals* would authorize the multisig custodians to take action defined textually in the proposal metadata. The proposal's validator would allow the custodians to implement the actions described in the text of proposal. This would be fallback that places trust in the validators to faithfully, manually execute the intent of the proposal. It is similar to the "DAO-owned multsig" and "community multisig" concepts.

```diff
- We could use NFT-based authorization instead of public-key hashes for the
- custodians, where the NFTs are minted for particular custodians and then
- expire. Alternatively, Plutus scripts could act as authorizers and mediate
- the multisig authorization. Existing Cardano capabilities seem sufficient,
- but there are several options here.
```

# Proposal Execution

The execution of parametric, distribution, and upgrade proposals would proceed automatically, with anyone submitting the required transactions and with the governance logic authorizing the proposal's transactions. Some upgrade proposals might require the multisig custodians to also sign some of those transactions, but the result of the proposal execution would be defined and constrained by the proposal's validators.

The execution of a proposal for altering governance or market parameters necessarily revises the relevant governance or market state. Execution of funds-distribution proposal results in transactions that update the state of the treasury and send `LQx` to specified addresses. The execution of a system-upgrade proposal may have far-ranging effects such as replacing all validators and revising all parameters or it may have a limited effect such as replacing a single validator whose interface with the rest of the system remains unchanged.

Any proposal has the option to specify that the LiqwidX state will be frozen for a specified period of time when the proposal is executed. This is generally unnecessary for parametric and distribution proposals. It may be necessary for the more complicated upgrade proposals, but simple replacement of a validator may not require such a freeze. Custodian proposals would almost certainly specify a freeze.

Similar, any proposal has the option to remove all other unexecuted proposals from all phases of the proposal pipeline. This likely would not be needed for parametric and distribution proposals, but upgrade and custodian proposals would almost certainly want to clear the proposal pipeline, thus avoiding potentially adverse interactions between incompatible proposals that all move to the execution phase.

```diff
- Here we could discuss the general-purpose design patterns that would enable
- proposal execution, but that might be better handled in the eUTxO specs.
```
