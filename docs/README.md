| Spec Status | Impl Status    | Last Revision |
|-------------|----------------|---------------|
| WIP         |  WIP           | v0.1 22/01/26 |

--------------------

**Spec Ownership:** [Jack Hodgkinson](github.com/jhodgdev)

**Authors**: [Jack Hodgkinson](github.com/jhodgdev)

**Current Status**:

--------------------
# Agora specification and documentation

This folder contains all of the specification and architecture documentation of Agora.

## Technical design

The `tech-design/` subdirectory contains high level descriptions of architecture involved in building Agora's governance system.

## Implementation

Agora makes extensive use of Plutarch. So it's prerequisite for understanding the implementation. Plutarch features an [_extensive_ guide](https://github.com/Plutonomicon/plutarch/blob/master/docs/GUIDE.md), which explains many intricacies of Plutarch and its use.

## Glossary

The following is a list of terms that are used frequently:

- **Governance Token (GT)**: The token that holds value within the protocol and is used for voting, rewards, etc. _Examples: Liqwid's LQ_.
- **Authority Token (GAT)**: A token that delegates authority of a particular script / token. See [tech-design/authority-token.md](./tech-design/authority-tokens.md)
- **Effect**: The result of a proposal, enforced by an effect script.
