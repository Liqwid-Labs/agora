# Agora specification and documentation

This folder contains documents explaining the conceptual background and technical implementation of Agora components.

## Technical design

The `tech-design/` subdirectory contains high level descriptions of the architecture of Agora's governance solution.

## Plutarch

Agora makes extensive use of [Plutarch](https://github.com/plutonomicon/plutarch). One unfamiliar with the library will be unable to suitably understand the technical parts of this documentation. The maintainers provide an extensive [guide](https://github.com/Plutonomicon/plutarch/blob/master/docs/GUIDE.md) that will familiarise the developer with the language and thereby this set of documentation.

## Glossary

The following is a list of terms that are used frequently throughout the documentation:

-   **DAO**: decentralised autonomous organisation.
-   **Proposal**: a set of changes to a Cardano protocol, suggested by a community member. Will be enacted, if passed by the community.
-   **Governance token (GT)**: the token that confers the right to vote on proposals within the protocol. May affect the user's eligibility for rewards. Examples include Liqwid's LQ.
-   **Governance authority token (GAT)**: A token that grant's the effects of a proposal the authority to alter the system. More information can be read [here](./tech-design/authority-tokens.md).
-   **Effect**: A script for implementing changes suggested by a proposal. An effect can make numerous changes and a proposal may have multiple effects.
