# Effects technical design

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

First draft of effect article. Subject to review from @emiflake.

---

[Proposals](proposals.md) in a governance system are necessarily going to perform some actions or alter some parameters (otherwise, what's the point?). Due to script size limitations on Cardano, it is difficult to encapsulate all of a proposal's effects alongside other necessary information, such as vote counts. Rather than be constrained by such limitations, Agora _decouples_ proposals from their effects.

Effects will exist as their own scripts on the blockchain and proposals will simply include a list of an effect's hashes. This way, users are still able to identify the changes a given proposal would make to the system and thereby assess the proposal's desirability.

A proposal's effects will be initiated by the community, whom we assume will have sufficient incentive to pay the required transaction fee. However, if these effects are independent scripts, sitting there waiting to be initiated, how can we trust community members to only activate the effects of _passed_ proposals?

We don't. Effects will be _unable_ to alter the system without burning [_governance authority tokens_ (GATs)](authority-tokens.md). These are bestowed upon an effect, via the [governor](governor.md), if their associated proposal has been passed by the community.

All 'governable' components of a system must be able to interface with effects and allow them to make necessary changes, so long as they are in possession of a GAT.

## What can an effect _be_?

The range of powers an effect may have is at the discretion of the protocol maker. For usability and security purposes though, Agora will initially offer a _buffet approach_ to proposals and effects.

<p align="center">
  <img height=300 src="https://images.unsplash.com/photo-1583338917496-7ea264c374ce?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1740&q=80" alt="Get the effects, whilst they're hot!"/> 
</p>

In this model, a proposal is defined by the _combination_ of _effect templates_, in much the same way a diner's meal at a buffet is defined by which dishes they choose. 

Conceiving of proposals this way makes them no less powerful, as they will be able to combine effects in such a way as to render the desired effect. It does however prevent a user wishing to issue a proposal from having to construct their effects from whole cloth and it allows the maintainers of the system a better insight into changes to the protocol a proposal might make.

## The issue of partial execution

An anticipated problem with this model is the danger of 'partial execution'. The model relies on the assumption that desired effects will be processed by community members, as they are seen as desirable.There could however be an issue, if users deem some effects as more desirable than others. If the effects of a proposal are not executed __in their entirety__, this may lead to unanticipated and undesirable outcomes.

This should not be a major limitation in the system, as community members _should_ recognise the necessity to implement the proposal in its entirety. However, one might consider _incentivising effect execution_ to prevent such an occurrence.
