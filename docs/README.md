# agora specification & documentation

This folder contains all of the specification and architecture documentation of agora.

## Technical design

The `tech-design/` subdirectory contains high level description of architecture involved in building agora's governance system.

## Implementation

Agora makes extensive use of plutarch. So it's prerequisite for understanding the implementation. Plutarch features an [_extensive_ guide](https://github.com/Plutonomicon/plutarch/blob/master/docs/GUIDE.md), which explains many intricacies of plutarch and its use.

## Glossary

The following is a list of words/shorthands that are frequently used:

- **Governance Token (GT)**: The token that holds value within the protocol and is used for voting, rewards, etc. _Examples: Liqwid's LQ_.
- **Authority Token (GAT)**: A token that delegates authority of a particular script / token. See [tech-design/authority-token.md](..)
